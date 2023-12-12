-- | This module provides servant authentication via the AuthProtect type.
-- It provides three individual tags that may be used to protect endpoints:
--   - ReqNoUser: No user must be logged in; otherwise an exception is raised.
--   - ReqOptionalUser: As if no AuthProtect type was provided, but the user information
--                      becomes available in the handler.
--   - ReqAuthorizedUser: The user must be logged in and have the specified permissions,
--                        otherwise an exception is raised.

module Buki.StaticFrontend.Core.Auth where

import Buki.Model (SessionId, Id (Id))

import Network.Wai (Request (..))
import Servant.Server.Experimental.Auth
import Web.Cookie (parseCookies)
import Buki.Backend.Session (authorize, HasValidatePermissions')
import qualified Buki.Err as B (Err(..))
import Buki.StaticFrontend.Core.AppM
import Servant.API.Experimental.Auth
import Buki.Backend.Auth
import Data.UUID (fromASCIIBytes)
import Effectful (liftIO)

data ReqNoUser
data ReqOptionalUser
data ReqAuthorizedUser (permissions :: [AuthorizationPermission])

type instance AuthServerData (AuthProtect ReqNoUser) = ()
type instance AuthServerData (AuthProtect ReqOptionalUser) = Maybe AuthorizedUser
type instance AuthServerData (AuthProtect (ReqAuthorizedUser permissions)) =
  Authorization permissions

authReqNoUser :: AppContext -> AuthHandler Request ()
authReqNoUser ctx = mkAuthHandler $ \req -> runAppM ctx $ do
  user <- authTryUser @'[] req
  case user of
    Nothing -> pure ()
    Just _ -> undefined -- throwErr $ Err 403 "You must not be logged in to access this resource."

authReqOptionalUser :: AppContext -> AuthHandler Request (Maybe AuthorizedUser)
authReqOptionalUser ctx = mkAuthHandler $ \req -> runAppM ctx $ do
  u <- authTryUser @'[] req
  pure $ u >>= unwrap
  where
    unwrap :: Authorization '[] -> Maybe AuthorizedUser
    unwrap (Authorization user) = Just user

authReqAuthorizedUser :: forall (permissions :: [AuthorizationPermission]). HasValidatePermissions' permissions => AppContext -> AuthHandler Request (Authorization permissions)
authReqAuthorizedUser ctx = mkAuthHandler $ \req -> runAppM ctx $ do
  liftIO $ putStrLn $ "Trying to authorize user with request " <> show req <> " ;;; " <> show (requestHeaders req)
  user <- authTryUser @permissions req
  liftIO $ putStrLn $ "User: " <> show user
  case user of
    Nothing -> undefined -- throwErr $ Err 403 "You must be logged in to access this resource."
    Just user' -> pure user'

authTryUser :: forall (permissions :: [AuthorizationPermission]). HasValidatePermissions' permissions => Request -> AppM (Maybe (Authorization permissions))
authTryUser req = do
  liftIO $ putStrLn $ "Trying to authorize user with request " <> show req <> " ;;; " <> show (requestHeaders req)
  case extractSessionToken req of
    Nothing -> pure Nothing
    Just sessionToken -> do
      liftIO $ putStrLn $ "Trying to authorize user with session token " <> show sessionToken
      mUser <- runEffects $ authorize sessionToken
      case mUser of
        (B.Failure _) -> pure Nothing
        (B.Success user) -> pure $ Just user

extractSessionToken :: Request -> Maybe SessionId
extractSessionToken req = do
  cookieHeader <- lookup "cookie" $ requestHeaders req
  sessionToken <- lookup "session_token" $ parseCookies cookieHeader
  uuid <- fromASCIIBytes sessionToken
  pure $ Id uuid
