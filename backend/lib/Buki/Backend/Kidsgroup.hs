{-# LANGUAGE Arrows #-}

module Buki.Backend.Kidsgroup where

import Control.Lens

import Data.Text (Text)
import Data.UUID (UUID)

import Effectful

import Buki.Backend.Auth
import Buki.Eff.Db
import Buki.Err

import Buki.Model qualified as M
import Buki.Types
import Control.Arrow (returnA)
import Opaleye qualified as O
import Opaleye.Operators

data ListKidsgroup = ListKidsgroup
    { listKidsgroupId :: M.Id M.Kidsgroup'
    , listKidsgroupName :: Name
    }
    deriving (Show, Eq)

newtype InvalidKidsgroupIdError = InvalidKidsgroupIdError UUID
    deriving (Show, Eq)
newtype KidsgroupNotEmptyError = KidsgroupNotEmptyError M.KidsgroupId
    deriving (Show, Eq)
newtype KidsgroupAlreadyExistsError = KidsgroupAlreadyExistsError M.Kidsgroup
    deriving (Show, Eq)
newtype KidsgroupNotFoundError = KidsgroupNotFoundError Name
    deriving (Show, Eq)

listKidsgroups ::
    forall es.
    (Db :> es) =>
    Eff es [ListKidsgroup]
listKidsgroups = do
    kidsgroups <- dbSelect @M.Kidsgroup $ O.selectTable M.kidsgroupTable
    pure $ map (\kidsgroup -> ListKidsgroup (kidsgroup ^. M.id) (kidsgroup ^. M.name)) kidsgroups

usersInKidsgroup :: forall auth es. (Db :> es, auth `HasPermissions` '[ 'UserManagement]) => auth -> UUID -> Eff es (Err '[InvalidKidsgroupIdError] [M.Id M.User'])
usersInKidsgroup _ kidsgroupId = do
    userIds <- dbSelect $ do
        users <- O.selectTable M.userTable
        O.where_ $ (users ^. M.id) .== O.toFields kidsgroupId
        pure $ users ^. M.id
    pure $ mkSuccess userIds

createKidsgroup ::
    forall auth es.
    (Db :> es, auth `HasPermissions` '[ 'UserManagement]) =>
    auth ->
    Name ->
    Eff es (Err '[KidsgroupAlreadyExistsError] M.Kidsgroup)
createKidsgroup _ name = do
    uuid <- dbMkUuid
    dbCatchViolation (onAnyUniqueViolationDo (findKidsgroupByName name >>= unwrapM' "" >>= pure . mkFailure . KidsgroupAlreadyExistsError))
        $ mkSuccess
        <$> dbInsertOne M.kidsgroupTable (O.toFields $ M.Kidsgroup uuid name)

findKidsgroupByName ::
    forall es.
    (Db :> es) =>
    Name ->
    Eff es (Err '[KidsgroupNotFoundError] M.Kidsgroup)
findKidsgroupByName name =
    do
        dbSelectAtMostOneForced $ proc () -> do
            kidsgroup <- O.selectTable M.kidsgroupTable -< ()
            O.restrict -< (kidsgroup ^. M.name) .== O.toFields name
            returnA -< kidsgroup
        >>= liftE (\NoRecords -> pure $ Left $ KidsgroupNotFoundError name)

renameKidsgroup ::
    forall auth es.
    (Db :> es, auth `HasPermissions` '[ 'UserManagement]) =>
    auth ->
    UUID ->
    Text ->
    Eff es (Err '[InvalidKidsgroupIdError, KidsgroupAlreadyExistsError] ())
renameKidsgroup _ kidsgroupId name = do
    _ <-
        dbUpdate
            $ O.Update
                { O.uTable = M.kidsgroupTable
                , O.uUpdateWith = O.updateEasy $ \kidsgroup -> kidsgroup & M.name .~ O.toFields name
                , O.uWhere = \kidsgroup -> (kidsgroup ^. M.id) .== O.toFields kidsgroupId
                , O.uReturning = O.rCount
                }
    pure $ mkSuccess ()

removeEmptyKidsgroup ::
    forall auth es.
    (Db :> es, auth `HasPermissions` '[ 'UserManagement]) =>
    auth ->
    UUID ->
    Eff es (Err '[InvalidKidsgroupIdError, KidsgroupNotEmptyError] M.Kidsgroup)
removeEmptyKidsgroup _ kidsgroupId = do
    dbCatchViolation (onAnyForeignKeyViolation $ mkFailure $ KidsgroupNotEmptyError $ M.Id kidsgroupId)
        $ dbDeleteOne M.kidsgroupTable (\k -> k ^. M.id .== O.toFields kidsgroupId)
        >>= liftE @'[InvalidKidsgroupIdError, KidsgroupNotEmptyError]
            (\NoRecords -> pure $ Left $ InvalidKidsgroupIdError kidsgroupId)
