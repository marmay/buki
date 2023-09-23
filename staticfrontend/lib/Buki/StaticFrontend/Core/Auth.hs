-- | This module provides servant authentication via the AuthProtect type.
-- It provides three individual tags that may be used to protect endpoints:
--   - ReqNoUser: No user must be logged in; otherwise an exception is raised.
--   - ReqOptionalUser: As if no AuthProtect type was provided, but the user information
--                      becomes available in the handler.
--   - ReqAuthorizedUser: The user must be logged in and have the specified permissions,
--                        otherwise an exception is raised.
module Buki.StaticFrontend.Core.Auth where

import Buki.Model.Types (SessionId, Id (Id))

import Network.Wai (Request (..))
import Servant.Server.Experimental.Auth
import Web.Cookie (parseCookies)
import Buki.Backend.Session (authorize, runSessionDb)
import qualified Buki.Err as B (Err(..))
import Buki.StaticFrontend.Core.AppM
import Servant.API.Experimental.Auth
import Buki.Backend.Auth
import Data.UUID (fromASCIIBytes)
import Data.Default (def)
import Buki.Backend.User (runUserDb)

data ReqNoUser
data ReqOptionalUser
data ReqAuthorizedUser (permissions :: [AuthorizationPermission])

type instance AuthServerData (AuthProtect ReqNoUser) = ()
type instance AuthServerData (AuthProtect ReqOptionalUser) = Maybe (Authorization '[])
type instance AuthServerData (AuthProtect (ReqAuthorizedUser permissions)) =
  Authorization permissions

authReqNoUser :: AppContext -> AuthHandler Request ()
authReqNoUser ctx = mkAuthHandler $ \req -> runAppM ctx $ do
  user <- authTryUser @'[] req
  case user of
    Nothing -> pure ()
    Just _ -> undefined -- throwErr $ Err 403 "You must not be logged in to access this resource."

authReqOptionalUser :: AppContext -> AuthHandler Request (Maybe (Authorization '[]))
authReqOptionalUser ctx = mkAuthHandler $ \req -> runAppM ctx $ authTryUser @'[] req

authReqAuthorizedUser :: forall (permissions :: [AuthorizationPermission]). AppContext -> AuthHandler Request (Authorization permissions)
authReqAuthorizedUser ctx = mkAuthHandler $ \req -> runAppM ctx $ do
  user <- authTryUser @permissions req
  case user of
    Nothing -> undefined -- throwErr $ Err 403 "You must be logged in to access this resource."
    Just user' -> pure user'

authTryUser :: forall (permissions :: [AuthorizationPermission]). Request -> AppM (Maybe (Authorization permissions))
authTryUser req = do
  case extractSessionToken req of
    Nothing -> pure Nothing
    Just sessionToken -> do
      mUser <- runEffects $ runUserDb $ runSessionDb def $ authorize sessionToken
      case mUser of
        (B.Failure _) -> pure Nothing
        (B.Success user) -> pure $ Just user

extractSessionToken :: Request -> Maybe SessionId
extractSessionToken req = do
  cookieHeader <- lookup "cookie" $ requestHeaders req
  sessionToken <- lookup "session_token" $ parseCookies cookieHeader
  uuid <- fromASCIIBytes sessionToken
  pure $ Id uuid
