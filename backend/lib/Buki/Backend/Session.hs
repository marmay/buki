{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module Buki.Backend.Session where

import Buki.Backend.Auth
import Buki.Backend.User
import Buki.Err
import Buki.Types

import Effectful
import Effectful.TH

import Buki.Eff.Db (Db, dbInsert1, dbMkUuid, dbSelect1, dbUpdate, dbDelete)
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

-- | User session manipulation backend.
data Session :: Effect where
  -- | Create a new session for the given user.
  MakeSession ::
    EmailAddress ->
    Password ->
    Session m (Err '[InvalidUserOrPasswordError] (M.SessionId, AuthorizedUser))

  -- | Destroy the session of the authenticated user.
  DestroyOwnSession ::
    forall auth m. auth `HasPermissions` '[] =>
    auth -> Session m Bool

  -- | Destroy the session of the given user.
  DestroyUserSession ::
    forall auth m. auth `HasPermissions` '[ 'UserManagement ] =>
    auth -> M.UserId -> Session m Bool

  -- | Authorize 
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

defaultSessionSettings :: SessionSettings
defaultSessionSettings = def

runSessionDb ::
  (Db :> es, Time :> es, User :> es) =>
  SessionSettings ->
  Eff (Session ': es) a ->
  Eff es a
runSessionDb SessionSettings{..} = interpret $ \_ -> \case
  MakeSession email password -> dbMakeSession email password sessionLength
  DestroyOwnSession auth -> dbDestroyOwnSession auth
  DestroyUserSession _ userId -> dbDestroyUserSessionFor userId
  Authorize sessionId -> dbAuthorize sessionId sessionLength

dbDestroyUserSessionFor :: forall es. (Db :> es) => M.UserId -> Eff es Bool
dbDestroyUserSessionFor userId = do
  count <- dbDelete $ O.Delete M.sessionTable (\session -> session ^. M.userId .== O.toFields userId) O.rCount
  pure $ count > 0

dbDestroyOwnSession :: forall auth es. auth `HasPermissions` '[] => (Db :> es) => auth -> Eff es Bool
dbDestroyOwnSession a = do
  case authUserId a (Proxy @'[]) of
    Nothing -> pure False
    Just userId -> dbDestroyUserSessionFor userId

dbMakeSession :: forall es. (Db :> es, Time :> es, User :> es) => EmailAddress -> Password -> NominalDiffTime -> Eff es (Err '[InvalidUserOrPasswordError] (M.SessionId, AuthorizedUser))
dbMakeSession email password sessionLength = do
  authenticateUser email password
    `mapWith` createSession
 where
  createSession (userId, name, permissions) = do
    _ <- dbDestroyUserSessionFor userId
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
    sessionId <- dbInsert1 M.sessionTable sessionRecord M.session'Id
    let authorizedUser = AuthorizedUser userId name email (toAuthPermissions permissions)
    pure $ mkSuccess (sessionId, authorizedUser)

dbAuthenticate :: forall es. (Db :> es, Time :> es) => M.SessionId -> NominalDiffTime -> Eff es (Err '[InvalidSessionId, SessionTimedOut] AuthorizedUser)
dbAuthenticate sessionId sessionLength = do
  selectSession
    >>= liftS @'[InvalidSessionId, SessionTimedOut] validateSession
    >>= liftS @'[InvalidSessionId, SessionTimedOut] updateSession
    >>= liftS (pure . mkSuccess . makeAuthenticatedUser)
 where
  selectSession :: Eff es (Err '[InvalidSessionId] (M.UserId, Text, Text, Permissions, UTCTime))
  selectSession =
    constMapErrors InvalidSessionId
      <$> dbSelect1
        ( proc () -> do
            session <- O.selectTable M.sessionTable -< ()
            O.restrict -< M.session'Id session O..== O.toFields sessionId
            returnA
              -<
                ( session ^. M.userId
                , session ^. M.name
                , session ^. M.email
                , session ^. M.permissions
                , session ^. M.expiresAt
                )
        )

  validateSession p@(_, _, _, _, expiresAt) = do
    now <- getTime
    if now > expiresAt
      then pure $ mkFailure SessionTimedOut
      else pure $ mkSuccess p

  -- updateSession :: (M.UserId, Text, Text, Permissions, UTCTime) -> Eff es (Err '[InvalidSessionId, SessionTimedOut] (M.UserId, Text, Text, Permissions, UTCTime))
  updateSession p@(_, _, _, _, expiresAt) = do
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
    -- pure $ mkSuccess (name, emailAddress, permissions)
    pure $ mkSuccess p

  makeAuthenticatedUser (userId, name, emailAddress, permissions, _) =
    AuthorizedUser userId (forceValidate name) (forceValidate emailAddress) (toAuthPermissions permissions)

dbAuthorize :: forall ps es. (Db :> es, Time :> es, HasValidatePermissions' ps) => M.SessionId -> NominalDiffTime -> Eff es (Err '[InvalidSessionId, SessionTimedOut, LackOfPrivileges] (Authorization ps))
dbAuthorize sessionId sessionLength = do
  dbAuthenticate sessionId sessionLength
    `embeddingErrors` embed'
    `guardWith` validatePermissions
    `mapWith` makeAuthorization
 where
  embed' :: AuthorizedUser -> Eff es (Err '[InvalidSessionId, SessionTimedOut, LackOfPrivileges] AuthorizedUser)
  embed' = pure . mkSuccess

  validatePermissions :: AuthorizedUser -> Eff es (Maybe LackOfPrivileges)
  validatePermissions (AuthorizedUser _ _ _ permissions) = do
    if validatePermissions' (Proxy @ps) permissions
      then pure Nothing
      else pure $ Just LackOfPrivileges

  makeAuthorization :: forall errs. AuthorizedUser -> Eff es (Err errs (Authorization ps))
  makeAuthorization a = do
    pure $ mkSuccess $ Authorization a

toAuthPermissions :: Permissions -> S.Set AuthorizationPermission
toAuthPermissions Admin      = S.fromList [minBound .. maxBound]
toAuthPermissions Registered = S.empty
toAuthPermissions _          = S.empty
