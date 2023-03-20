module Buki.Eff.Db
  ( Db(..)
  , dbSelect
  , dbInsert
  , dbInsert1
  , dbUpdate
  , dbDelete
  , dbCatch
  , dbFetch
  , dbMkUuid
  , dbWithTransaction
  , runDb
  , dbSelect1
  ) where

import Data.UUID (UUID)

import Effectful
import Effectful.TH
import Effectful.Dispatch.Dynamic

import qualified Opaleye as O
import qualified Data.Profunctor.Product.Default as D
import qualified Database.PostgreSQL.Simple as S
import Data.UUID.V4 (nextRandom)
import qualified Database.PostgreSQL.Simple.Errors as S
import Buki.Err
import Control.Exception (try, throw)

data NoRecordsFound = NoRecordsFound
  deriving (Eq, Show)
data TooManyRecordsFound = TooManyRecordsFound
  deriving (Eq, Show)

-- In case of database manipulation, we want to handle constraint violations.
-- This seems to be the best way to detect and report many domain errors.
--
--
data Db :: Effect where
  DbSelect :: forall haskells fields m.
              ( D.Default O.FromFields fields haskells
              , D.Default O.Unpackspec fields fields
              )
           => O.Select fields -> Db m [haskells]
  DbFetch  :: forall haskells fields m.
              ( D.Default O.FromFields fields haskells
              , D.Default O.Unpackspec fields fields
              )
           => O.Select fields -> Db m (Maybe haskells)
  DbInsert :: O.Insert haskells -> Db m haskells
  DbUpdate :: O.Update haskells -> Db m haskells
  DbDelete :: O.Delete haskells -> Db m haskells
  DbInsert1 :: forall haskells fields fieldsR fieldsW m.
              ( D.Default O.FromFields fields haskells
              )
           => O.Table fieldsW fieldsR -> fieldsW -> (fieldsR -> fields) -> Db m haskells
  DbCatch :: (S.ConstraintViolation -> Maybe (Err errs a)) -> m a -> Db m (Err errs a)
  DbMkUuid :: Db m UUID
  DbWithTransaction :: m a -> Db m a

makeEffect ''Db

runDb :: forall es a. (HasCallStack, IOE :> es) => S.Connection -> Eff (Db ': es) a -> Eff es a
runDb conn = interpret $ \env -> \case
  DbSelect select -> liftIO $ O.runSelect conn select
  DbInsert insert -> liftIO $ O.runInsert conn insert
  DbUpdate update -> liftIO $ O.runUpdate conn update
  DbDelete delete -> liftIO $ O.runDelete conn delete
  DbInsert1 table fields projection -> liftIO $ do
    res <- O.runInsert conn O.Insert { O.iTable = table
                                      , O.iRows = [fields]
                                      , O.iReturning = O.rReturning projection
                                      , O.iOnConflict = Nothing
                                      }
    case res of
      [a] -> pure a
      _ -> error "DbInsert1: unexpected result"
  DbCatch handler action -> localSeqUnliftIO env $ \unlift -> do
    r <- liftIO $ try $ unlift action
    case r of
      Left e -> do
        case S.constraintViolation e of
          Nothing -> liftIO $ throw e
          Just cv -> do
            case handler cv of
              Nothing -> liftIO $ throw e
              Just r'' -> pure r''
      Right a -> pure $ pure a
  DbFetch select -> do
    rows <- liftIO $ O.runSelect conn select
    case rows of
      [row] -> pure $ Just row
      [] -> pure Nothing
      _ -> pure Nothing -- TODO: throw error
  DbMkUuid -> liftIO nextRandom
  DbWithTransaction action -> localSeqUnliftIO env $ \unlift -> do
    liftIO $ S.begin conn -- TODO: handle exceptions
    r <- unlift action
    liftIO (S.rollback conn)
    pure r

dbSelect1 :: forall haskells fields es.
              ( D.Default O.FromFields fields haskells
              , D.Default O.Unpackspec fields fields
              , Db :> es
              )
           => O.Select fields -> Eff es (Err '[NoRecordsFound, TooManyRecordsFound] haskells)
dbSelect1 select = do
  rows <- dbSelect select
  case rows of
    [row] -> pure $ mkSuccess row
    [] -> pure $ mkFailure NoRecordsFound
    _ -> pure $ mkFailure TooManyRecordsFound
