module Buki.Eff.Db
  ( Db(..)
  , NoRecordsFound(..)
  , TooManyRecordsFound(..)
  , ConstraintViolationHandler(..)
  , dbSelect
  , dbInsert
  , dbInsert1
  , dbUpdate
  , dbDelete
  , dbDelete1
  , dbCatch
  , dbCatchErr
  , dbFetch
  , dbMkUuid
  , dbWithTransaction
  , dbWithTransactionErr
  , runDb
  , dbSelect1
  , dbSelect1'
  , dbSelect1Maybe
  , onAnyUniqueViolation
  , onAnyForeignKeyViolation
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

newtype ConstraintViolationHandler errs m a = ConstraintViolationHandler {
    runConstraintViolationHandler :: S.ConstraintViolation -> m (Maybe (Err errs a))
  }

instance (Monad m) => Semigroup (ConstraintViolationHandler errs m a) where
  ConstraintViolationHandler a <> ConstraintViolationHandler b = ConstraintViolationHandler $ \cv -> do
    r <- a cv
    case r of
      Nothing -> b cv
      Just r' -> pure $ Just r'

instance (Monad m) => Monoid (ConstraintViolationHandler errs m a) where
  mempty = ConstraintViolationHandler $ \_ -> pure Nothing

onAnyUniqueViolation :: forall errs m a. (Monad m) => Err errs a -> ConstraintViolationHandler errs m a
onAnyUniqueViolation err = ConstraintViolationHandler $
  \case (S.UniqueViolation _) -> pure $ Just err
        _                     -> pure Nothing

onAnyForeignKeyViolation :: forall err errs m a. (Monad m, err `In` errs) => err -> ConstraintViolationHandler errs m a
onAnyForeignKeyViolation err = ConstraintViolationHandler $
  \case (S.ForeignKeyViolation _ _) -> pure $ Just $ mkFailure err
        _                           -> pure Nothing

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
  DbDelete1 :: forall haskells fields fieldsR fieldsW m.
               ( D.Default O.FromFields fields haskells
               )
            => O.Table fieldsW fieldsR -> (fieldsR -> O.Field O.SqlBool) -> (fieldsR -> fields)
               -> Db m (Err '[NoRecordsFound] haskells)
  DbInsert1 :: forall haskells fields fieldsR fieldsW m.
              ( D.Default O.FromFields fields haskells
              )
           => O.Table fieldsW fieldsR -> fieldsW -> (fieldsR -> fields) -> Db m haskells
  DbCatch :: ConstraintViolationHandler errs m a -> m (Err errs a) -> Db m (Err errs a)
  DbCatchErr :: (S.ConstraintViolation -> m (Maybe (Err errs a))) -> m a -> Db m (Err errs a)
  DbMkUuid :: Db m UUID
  DbWithTransaction :: m a -> Db m a
  DbWithTransactionErr :: m (Err errs a) -> Db m (Err errs a)

makeEffect ''Db

runDb :: forall es a. (HasCallStack, IOE :> es) => S.Connection -> Eff (Db ': es) a -> Eff es a
runDb conn = interpret $ \env -> \case
  DbSelect select -> liftIO $ O.runSelect conn select
  DbInsert insert -> liftIO $ O.runInsert conn insert
  DbUpdate update -> liftIO $ O.runUpdate conn update
  DbDelete delete -> liftIO $ O.runDelete conn delete
  DbDelete1 table predicate projection -> liftIO $ do
    res <- O.runDelete conn O.Delete { O.dTable = table
                                     , O.dWhere = predicate
                                     , O.dReturning = O.rReturning projection
                                     }
    case res of
      [a] -> pure $ mkSuccess a
      []  -> pure $ mkFailure NoRecordsFound
      _   -> error "DbDelete1: unexpected result"
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
            handlerResult <- unlift $ runConstraintViolationHandler handler cv
            case handlerResult of
              Nothing -> liftIO $ throw e
              Just r'' -> pure r''
      Right a -> pure a
  DbCatchErr handler action -> localSeqUnliftIO env $ \unlift -> do
    r <- liftIO $ try $ unlift action
    case r of
      Left e -> do
        case S.constraintViolation e of
          Nothing -> liftIO $ throw e
          Just cv -> do
            handlerResult <- unlift $ handler cv
            case handlerResult of
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
    liftIO (S.commit conn)
    pure r
  DbWithTransactionErr action -> localSeqUnliftIO env $ \unlift -> do
    liftIO $ S.begin conn -- TODO: handle exceptions
    r <- unlift action
    case r of
      Failure _ -> liftIO (S.rollback conn)
      _         -> liftIO (S.commit conn)
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

dbSelect1' :: forall haskells fields es.
              ( D.Default O.FromFields fields haskells
              , D.Default O.Unpackspec fields fields
              , Db :> es
              )
           => O.Select fields -> Eff es (Err '[NoRecordsFound] haskells)
dbSelect1' select = do
  rows <- dbSelect select
  case rows of
      [row] -> pure $ mkSuccess row
      [] -> pure $ mkFailure NoRecordsFound
      _ -> error "dbSelect1': too many rows"

dbSelect1Maybe :: forall haskells fields es.
              ( D.Default O.FromFields fields haskells
              , D.Default O.Unpackspec fields fields
              , Db :> es
              )
           => O.Select fields -> Eff es (Maybe haskells)
dbSelect1Maybe select =
  dbSelect1' select
    >>= liftS (pure . mkSuccess . Just)
    >>= liftF (\NoRecordsFound -> pure (Nothing :: Maybe haskells))
    >>= unwrapM
