-- | Provide an Opaleye-based database Effect.
--
-- This module provides the essentials for implementing database operations
-- with Opaleye and friends. It supports transactions, exception handling and
-- executing all kinds of queries.
--
-- Most operations are re-exported from `Buki.Eff.Db`.
module Buki.Eff.Db.Eff (
    Db,
    dbMkUuid,
    dbSelect,
    dbInsert,
    dbUpdate,
    dbDelete,
    dbWithTransactionPred,
    dbCatchViolation,
    runDb,
) where

import Effectful
import Effectful.Dispatch.Dynamic (HasCallStack, interpret, localSeqUnliftIO)
import Effectful.TH

import Data.Profunctor.Product.Default qualified as D
import Opaleye qualified as O

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)

import Buki.Eff.Db.Catch (ConstraintViolationHandler (..))
import Control.Monad.Catch (SomeException, catch, throwM, try)
import Database.PostgreSQL.Simple qualified as S
import Database.PostgreSQL.Simple.Errors qualified as S

-- In case of database manipulation, we want to handle constraint violations.
-- This seems to be the best way to detect and report many domain errors.
data Db :: Effect where
    -- | Generates a new UUID; while strictly not database related, it is often
    --   helpful to create one in the Haskell code instead of letting Postgres
    --   take care of it.
    DbMkUuid :: Db m UUID
    -- | Runs an Opaleye query and returns the results.
    DbSelect ::
        forall haskells fields m.
        ( D.Default O.FromFields fields haskells
        , D.Default O.Unpackspec fields fields
        ) =>
        O.Select fields ->
        Db m [haskells]
    -- | Runs an Opaleye insert command.
    DbInsert :: O.Insert haskells -> Db m haskells
    -- | Runs an Opaleye update command.
    DbUpdate :: O.Update haskells -> Db m haskells
    -- | Runs an Opaleye deltete command.
    DbDelete :: O.Delete haskells -> Db m haskells
    -- | Evaluates an action within an SQL transaction and then evaluates
    --   whether it was successful. If the action is not successful or throws
    --   an exception, the transaction is rolled back; otherwise it is commited.
    DbWithTransactionPred :: m a -> (a -> Bool) -> Db m a
    -- | Catches certain constraint violations within an action and handles them.
    DbCatchViolation :: ConstraintViolationHandler m a -> m a -> Db m a

makeEffect ''Db

runDb :: forall es a. (HasCallStack, IOE :> es) => S.Connection -> Eff (Db ': es) a -> Eff es a
runDb conn = interpret $ \env -> \case
    DbMkUuid -> liftIO nextRandom
    DbSelect select -> liftIO $ O.runSelect conn select
    DbInsert insert -> liftIO $ O.runInsert conn insert
    DbUpdate update -> liftIO $ O.runUpdate conn update
    DbDelete delete -> liftIO $ O.runDelete conn delete
    DbWithTransactionPred action predicate -> localSeqUnliftIO env $ \unlift -> do
        liftIO
            ( do
                S.begin conn
                r <- unlift action
                if predicate r
                    then S.commit conn
                    else S.rollback conn
                pure r
            )
            `catch` ( \(e :: SomeException) -> liftIO $ do
                        S.rollback conn
                        throwM e
                    )
    DbCatchViolation handler action -> localSeqUnliftIO env $ \unlift -> do
        r <- liftIO $ try $ unlift action
        case r of
            Left e -> do
                case S.constraintViolation e of
                    Nothing -> liftIO $ throwM e
                    Just cv -> do
                        handlerResult <- unlift $ runConstraintViolationHandler handler cv
                        case handlerResult of
                            Nothing -> liftIO $ throwM e
                            Just r'' -> pure r''
            Right a -> pure a
