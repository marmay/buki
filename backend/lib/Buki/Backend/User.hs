{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module Buki.Backend.User where

import Effectful

import Control.Lens ((&), (.~), (^.))

import Data.Text (Text)
import Data.Time (LocalTime)

import Buki.Model.Permissions (Permissions (..))

import Buki.Types

import Opaleye qualified as O
import Opaleye.Operators

import Buki.Eff.Db
import Buki.Err
import Buki.Model qualified as M
import Buki.Validation

import Buki.Backend.Auth
import Control.Arrow (returnA)

data RegisterData = RegisterData
    { registerDataName :: Name
    , registerDataPassword :: Password
    , registerDataEmail :: EmailAddress
    , registerDataKidsgroup :: M.KidsgroupId
    , registerDataKidsymbol :: Text
    }
    deriving (Eq, Show)

data ListUser = ListUser
    { listUserName :: Name
    , listUserEmail :: EmailAddress
    , listUserLockedAt :: Maybe LocalTime
    , listUserKidsgroupId :: M.KidsgroupId
    , listUserKidsgroupName :: Name
    , listUserKidsymbol :: Text
    , listUserPermissions :: Permissions
    }
    deriving (Eq, Show)

data EmailAddressTakenError = EmailAddressTakenError
    deriving (Eq, Show)
data InvalidKidsgroupIdError = InvalidKidsgroupIdError
    deriving (Eq, Show)
data InvalidUserIdError = InvalidUserIdError
    deriving (Eq, Show)
data InvalidUserOrPasswordError = InvalidUserOrPasswordError
    deriving (Eq, Show)

registerUser :: forall es. (Db :> es) => RegisterData -> Eff es (Err '[EmailAddressTakenError, InvalidKidsgroupIdError] M.UserId)
registerUser RegisterData{..} = do
    userId <- dbMkUuid
    let userRecord =
            M.User
                { user'Id = O.toFields userId
                , user'Email = O.toFields registerDataEmail
                , user'Name = O.toFields registerDataName
                , user'PasswordHash = O.toFields $ unvalidate @Text registerDataPassword
                , user'FailedLoginAttempts = O.sqlInt4 0
                , user'LockedAt = O.nothingFields
                , user'Permissions = O.toFields Nobody
                , user'KidsgroupId = O.toFields registerDataKidsgroup
                , user'Kidsymbol = O.toFields registerDataKidsymbol
                }
    dbCatchViolation
        ( onAnyUniqueViolation (mkFailure EmailAddressTakenError)
            <> onAnyForeignKeyViolation (mkFailure InvalidKidsgroupIdError)
        )
        $ mkSuccess
        <$> dbInsertOneProj M.userTable userRecord M.user'Id

--    where
--      handler :: S.ConstraintViolation -> Maybe (Err '[EmailAddressTakenError, InvalidKidsgroupIdError] M.UserId)
--      handler (S.UniqueViolation _) = Just $ mkFailure EmailAddressTakenError
--      handler (S.ForeignKeyViolation _ _) = Just $ mkFailure InvalidKidsgroupIdError
--      handler _ = Nothing

authenticateUser :: (Db :> es) => EmailAddress -> Password -> Eff es (Err '[InvalidUserOrPasswordError] (M.UserId, Name, M.Permissions))
authenticateUser email password = do
    let query = proc () -> do
            user <- O.selectTable M.userTable -< ()
            O.restrict -< user ^. M.email .== O.toFields email
            O.restrict -< user ^. M.passwordHash .== O.toFields password
            returnA -< (user ^. M.id, user ^. M.name, user ^. M.permissions)
    res <- dbSelect query
    case res of
        [(userId, userName, userPermissions)] ->
            pure $ mkSuccess (userId, Name userName, userPermissions)
        _ -> pure $ mkFailure InvalidUserOrPasswordError

listUsers :: forall auth es. (Db :> es, auth `HasPermissions` '[ 'UserManagement]) => auth -> Eff es [ListUser]
listUsers _ = do
    ps <- dbSelect q
    pure $ fmap toListUser ps
  where
    q :: O.SelectArr () (M.UserField, M.KidsgroupField)
    q = proc () -> do
        user <- O.selectTable M.userTable -< ()
        kidsgroup <- O.selectTable M.kidsgroupTable -< ()
        O.restrict -< (user ^. M.kidsgroupId) .== (kidsgroup ^. M.id)
        returnA -< (user, kidsgroup)

    toListUser :: (M.User, M.Kidsgroup) -> ListUser
    toListUser (user, kidsgroup) =
        ListUser
            { listUserName = user ^. M.name
            , listUserEmail = user ^. M.email
            , listUserLockedAt = user ^. M.lockedAt
            , listUserKidsgroupId = kidsgroup ^. M.id
            , listUserKidsgroupName = kidsgroup ^. M.name
            , listUserKidsymbol = user ^. M.kidsymbol
            , listUserPermissions = user ^. M.permissions
            }

changeUserPermissions ::
    forall auth es.
    (Db :> es, auth `HasPermissions` '[ 'UserManagement]) =>
    auth ->
    M.UserId ->
    Permissions ->
    Eff es (Err '[InvalidUserIdError] ())
changeUserPermissions _ userId permissions = do
    updatedCount <-
        dbUpdate
            $ O.Update
                { O.uTable = M.userTable
                , O.uUpdateWith = \user -> user & M.permissions .~ O.toFields permissions
                , O.uWhere = \user -> user ^. M.id .== O.toFields userId
                , O.uReturning = O.rCount
                }

    case updatedCount of
        1 -> pure $ mkSuccess ()
        _ -> pure $ mkFailure InvalidUserIdError
