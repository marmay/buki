{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module Buki.Backend.Session where

import Buki.Backend.Auth
import Buki.Backend.User
import Buki.Err
import Buki.Types

import Effectful
import Effectful.TH

import Buki.Eff.Db (Db, dbInsert1, dbMkUuid, dbSelect1, dbUpdate)
import Buki.Eff.Time (Time, getTime)

import Data.Default (Default (..))
import qualified Data.Set as S
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Effectful.Dispatch.Dynamic (interpret)

import Buki.Model.Tables qualified as M
import Buki.Model.Types (Permissions)
import Buki.Model.Types qualified as M
import Buki.Model.Types.Permissions (Permissions (..))
import Buki.Validation (forceValidate)
import Control.Arrow
import Control.Lens ((&), (.~), (^.))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Opaleye qualified as O
import Opaleye.Operators ((.==))

data InvalidSessionId = InvalidSessionId
  deriving (Eq, Show)
data LackOfPrivileges = LackOfPrivileges
  deriving (Eq, Show)
data SessionTimedOut = SessionTimedOut
  deriving (Eq, Show)

class HasValidatePermissions' (ps :: [AuthorizationPermission]) where
  validatePermissions' :: Proxy ps -> S.Set AuthorizationPermission -> Bool
instance HasValidatePermissions' '[] where
  validatePermissions' _ _ = True
instance HasValidatePermissions' ps => HasValidatePermissions' ('UserManagement ': ps) where
  validatePermissions' _ permissions =
    S.member UserManagement permissions && validatePermissions' (Proxy @ps) permissions

data AuthenticatedUser = AuthenticatedUser
  { authenticatedUserName :: Name
  , authenticatedUserEmail :: EmailAddress
  , authenticatedUserPermissions :: Permissions
  }

data Session :: Effect where
  MakeSession ::
    EmailAddress ->
    Password ->
    Session m (Err '[InvalidUserOrPasswordError] M.SessionId)
  Authorize ::
    forall (ps :: [AuthorizationPermission]) m. HasValidatePermissions' ps =>
    M.SessionId ->
    Session m (Err '[InvalidSessionId, SessionTimedOut, LackOfPrivileges] (Authorization ps))

makeEffect ''Session
type instance DispatchOf Session = 'Dynamic

newtype SessionSettings = SessionSettings
  { sessionLength :: NominalDiffTime
  }
  deriving (Eq, Show)

instance Default SessionSettings where
  def =
    SessionSettings
      { sessionLength = 900
      }

runSessionDb ::
  (Db :> es, Time :> es, User :> es) =>
  SessionSettings ->
  Eff (Session ': es) a ->
  Eff es a
runSessionDb SessionSettings{..} = interpret $ \_ -> \case
  MakeSession email password -> dbMakeSession email password sessionLength
  Authorize sessionId -> dbAuthorize sessionId sessionLength

dbMakeSession :: forall es. (Db :> es, Time :> es, User :> es) => EmailAddress -> Password -> NominalDiffTime -> Eff es (Err '[InvalidUserOrPasswordError] M.SessionId)
dbMakeSession email password sessionLength = do
  authenticateUser email password
    `mapWith` createSession
 where
  createSession (userId, name, permissions) = do
    uuid <- M.Id <$> dbMkUuid
    now <- getTime
    let sessionRecord =
          M.Session
            { M.session'Id = O.toFields uuid
            , M.session'UserId = O.toFields userId
            , M.session'Name = O.toFields name
            , M.session'Email = O.toFields email
            , M.session'Permissions = O.toFields permissions
            , M.session'ExpiresAt = O.toFields $ addUTCTime sessionLength now
            }
    mkSuccess <$> dbInsert1 M.sessionTable sessionRecord M.session'Id

dbAuthenticate :: forall es. (Db :> es, Time :> es) => M.SessionId -> NominalDiffTime -> Eff es (Err '[InvalidSessionId, SessionTimedOut] AuthenticatedUser)
dbAuthenticate sessionId sessionLength = do
  selectSession
    `guardWith` validateSession
    `processWith` updateSession
    `mapWithPure` makeAuthenticatedUser
 where
  selectSession :: Eff es (Err '[InvalidSessionId, SessionTimedOut] (Text, Text, Permissions, UTCTime))
  selectSession =
    constMapErrors InvalidSessionId
      <$> dbSelect1
        ( proc () -> do
            session <- O.selectTable M.sessionTable -< ()
            O.restrict -< M.session'Id session O..== O.toFields sessionId
            returnA
              -<
                ( session ^. M.name
                , session ^. M.email
                , session ^. M.permissions
                , session ^. M.expiresAt
                )
        )

  validateSession (_, _, _, expiresAt) = do
    now <- getTime
    if now > expiresAt
      then pure $ Just SessionTimedOut
      else pure Nothing

  updateSession (name, emailAddress, permissions, expiresAt) = do
    _ <-
      dbUpdate
        O.Update
          { O.uTable = M.sessionTable
          , O.uUpdateWith = O.updateEasy
              $ \session ->
                session
                  & M.expiresAt
                    .~ O.toFields (addUTCTime sessionLength expiresAt)
          , O.uWhere = \session -> session ^. M.id .== O.toFields sessionId
          , O.uReturning = O.rCount
          }
    pure $ mkSuccess (name, emailAddress, permissions)

  makeAuthenticatedUser (name, emailAddress, permissions, _) =
    AuthenticatedUser (forceValidate name) (forceValidate emailAddress) permissions

dbAuthorize :: forall ps es. (Db :> es, Time :> es, HasValidatePermissions' ps) => M.SessionId -> NominalDiffTime -> Eff es (Err '[InvalidSessionId, SessionTimedOut, LackOfPrivileges] (Authorization ps))
dbAuthorize sessionId sessionLength = do
  dbAuthenticate sessionId sessionLength
    `embeddingErrors` embed'
    `guardWith` validatePermissions
    `mapWith` makeAuthorization
 where
  embed' :: AuthenticatedUser -> Eff es (Err '[InvalidSessionId, SessionTimedOut, LackOfPrivileges] AuthenticatedUser)
  embed' = pure . mkSuccess

  validatePermissions :: AuthenticatedUser -> Eff es (Maybe LackOfPrivileges)
  validatePermissions (AuthenticatedUser _ _ permissions) = do
    if validatePermissions' (Proxy @ps) (toAuthPermissions permissions)
      then pure Nothing
      else pure $ Just LackOfPrivileges

  makeAuthorization :: forall errs. AuthenticatedUser -> Eff es (Err errs (Authorization ps))
  makeAuthorization (AuthenticatedUser name emailAddress permissions) = do
    pure $ mkSuccess $ Authorization name emailAddress (toAuthPermissions permissions)

  toAuthPermissions :: Permissions -> S.Set AuthorizationPermission
  toAuthPermissions Admin      = S.fromList [minBound .. maxBound]
  toAuthPermissions Registered = S.empty
  toAuthPermissions _          = S.empty
