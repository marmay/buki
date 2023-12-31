{-# LANGUAGE RecordWildCards #-}

module Buki.StaticFrontend.User.Login.Controller where

import Buki.StaticFrontend.Core.AppM
import Buki.StaticFrontend.User.Login.Views (loginPage, loginSucceededPage, logoutSucceededPage)
import Buki.StaticFrontend.User.Login.Types (LoginError(..), LoginData(..))
import Buki.StaticFrontend.Core.Preludes.API
import Buki.Backend.Session
import Buki.Err (Err(..))
import Servant (ServerT)
import Buki.StaticFrontend.User.Login.API
import Buki.Backend.Auth (Authorization)
import Buki.Model (SessionId)
import Data.Text
import Effectful (liftIO)
import Buki.Model.Id (toUuid)

showUserLogin :: () -> AppM Html
showUserLogin () = liftViewM Nothing $ loginPage Nothing mempty

failUserLogin :: () -> LoginError -> FormValidationData -> AppM Html
failUserLogin () loginError formValidationData = liftViewM Nothing $ loginPage (Just loginError) formValidationData

handleUserLogin :: () -> FormValidation LoginData -> AppM (Headers '[Header "Set-Cookie" Text] Html)
handleUserLogin () (FormValidation formData Nothing) =
  noHeader <$> failUserLogin () LoginErrorInvalidInput formData
handleUserLogin () (FormValidation formData (Just LoginData{..})) = do
  liftIO $ putStrLn $ "LoginData: " ++ show loginDataEmail ++ " " ++ show loginDataPassword
  tryLogin >>= makeResponse
  where
    tryLogin = runEffects $ makeSession loginDataEmail loginDataPassword

    makeResponse (Success (sessionId, authorizedUser)) = do
      addHeader (encodeSessionId sessionId) <$> liftViewM (Just authorizedUser) loginSucceededPage
    makeResponse (Failure _) =
      noHeader <$> failUserLogin () LoginErrorInvalidCredentials formData

    encodeSessionId :: SessionId -> Text
    encodeSessionId sessionId = pack $ "session_token=" <> show (toUuid sessionId) <> "; Path=/"

handleUserLogout :: Authorization '[] -> AppM Html
handleUserLogout auth = do
  logout >>= makeResponse
  where
    logout = runEffects $ destroyOwnSession auth
    makeResponse sessionDestroyed =
      liftViewM Nothing $ logoutSucceededPage sessionDestroyed
    
userLoginServer :: ServerT UserLoginAPI AppM
userLoginServer = showUserLogin :<|> handleUserLogin :<|> handleUserLogout
