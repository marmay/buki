module Buki.Eff.Db.Util (
    NoRecords (..),
    TooManyRecords (..),
    dbSelectExactlyOne,
    dbSelectExactlyOneForced,
    dbSelectAtMostOneForced,
    dbInsertOneProj,
    dbInsertOne,
    dbDeleteOneProj,
    dbDeleteOne,
    dbWithTransaction,
    dbWithTransactionErr,
) where

import Effectful

import Data.Profunctor.Product.Default qualified as D
import Opaleye qualified as O

import Buki.Eff.Db.Eff
import Buki.Err
import Control.Lens ((<&>))

data NoRecords = NoRecords
    deriving (Eq, Show)
data TooManyRecords = TooManyRecords
    deriving (Eq, Show)

-- | Selects exactly one row from the database.
--
--   Returns NoRecords or TooManyRecords on errors.
dbSelectExactlyOne ::
    forall haskells fields es.
    ( D.Default O.FromFields fields haskells
    , D.Default O.Unpackspec fields fields
    , Db :> es
    ) =>
    O.Select fields ->
    Eff es (Err '[NoRecords, TooManyRecords] haskells)
dbSelectExactlyOne select = do
    rows <- dbSelect select
    case rows of
        [x] -> pure $ mkSuccess x
        [] -> pure $ mkFailure NoRecords
        _ -> pure $ mkFailure TooManyRecords

-- | Selects exactly one row from the database.
--
--   Throws an exception if there are no rows or if there is more than one row.
dbSelectExactlyOneForced ::
    forall haskells fields es.
    ( D.Default O.FromFields fields haskells
    , D.Default O.Unpackspec fields fields
    , Db :> es
    ) =>
    O.Select fields ->
    Eff es haskells
dbSelectExactlyOneForced select =
    dbSelectExactlyOne select
        >>= \case
            Success x -> pure x
            Failure e -> error $ "dbSelectExactlyOneForced: " <> show e

-- | Selects at most one row from the database.
--
--   Throws an exception if there is more than one row.
dbSelectAtMostOneForced ::
    forall haskells fields es.
    ( D.Default O.FromFields fields haskells
    , D.Default O.Unpackspec fields fields
    , Db :> es
    ) =>
    O.Select fields ->
    Eff es (Err '[NoRecords] haskells)
dbSelectAtMostOneForced select =
    dbSelectExactlyOne select
        >>= liftR @'[TooManyRecords, NoRecords]
        <&> forceUnwrapOne "dbSelectAtMostOneForced: too many rows"

-- | Inserts one single row into a database.
--
--   Throws an error if the insertion fails.
dbInsertOneProj ::
    forall haskells fields fieldsR fieldsW es.
    ( D.Default O.FromFields fields haskells
    , Db :> es
    ) =>
    O.Table fieldsW fieldsR ->
    fieldsW ->
    (fieldsR -> fields) ->
    Eff es haskells
dbInsertOneProj table fieldsW proj = do
    res <-
        dbInsert
            O.Insert
                { O.iTable = table
                , O.iRows = [fieldsW]
                , O.iReturning = O.rReturning proj
                , O.iOnConflict = Nothing
                }
    case res of
        [x] -> pure x
        _ -> error "dbInsertOneProj: insertion failed!"

-- | Inserts one single row into a database.
--
--   Throws an error if the insertion fails.
dbInsertOne ::
    forall haskells fieldsR fieldsW es.
    ( D.Default O.FromFields fieldsR haskells
    , Db :> es
    ) =>
    O.Table fieldsW fieldsR ->
    fieldsW ->
    Eff es haskells
dbInsertOne table fieldsW = dbInsertOneProj table fieldsW id

-- | Deletes one single row from the database.
--
--   Throws an error and rolls back if not exaclty one row was deleted.
dbDeleteOneProj ::
    forall haskells fields fieldsR fieldsW es.
    ( D.Default O.FromFields fields haskells
    , Db :> es
    ) =>
    O.Table fieldsW fieldsR ->
    (fieldsR -> O.Field O.SqlBool) ->
    (fieldsR -> fields) ->
    Eff es (Err '[NoRecords] haskells)
dbDeleteOneProj table predicate proj = dbWithTransactionErr $ do
    res <-
        dbDelete
            O.Delete
                { O.dTable = table
                , O.dWhere = predicate
                , O.dReturning = O.rReturning proj
                }
    case res of
        [] -> pure $ mkFailure NoRecords
        [x] -> pure $ mkSuccess x
        _ -> error "dbDeleteOneProj: deletion failed!"

-- | Deletes one single row from the database.
--
--   Throws an error and rolls back if not exaclty one row was deleted.
dbDeleteOne ::
    forall haskells fieldsR fieldsW es.
    ( D.Default O.FromFields fieldsR haskells
    , Db :> es
    ) =>
    O.Table fieldsW fieldsR ->
    (fieldsR -> O.Field O.SqlBool) ->
    Eff es (Err '[NoRecords] haskells)
dbDeleteOne table predicate = dbDeleteOneProj table predicate id

dbWithTransaction :: (Db :> es) => Eff es a -> Eff es a
dbWithTransaction a = dbWithTransactionPred a (const True)

dbWithTransactionErr :: (Db :> es) => Eff es (Err errs a) -> Eff es (Err errs a)
dbWithTransactionErr a = dbWithTransactionPred a (\case Success _ -> True; Failure _ -> False)
