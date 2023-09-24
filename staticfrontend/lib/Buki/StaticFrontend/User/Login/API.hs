module Buki.StaticFrontend.User.Login.API where

import Buki.StaticFrontend.Core.Preludes.API
import Buki.StaticFrontend.User.Login.Types

import Data.Text (Text)

type ShowUserLoginRoute =
  AuthProtect ReqNoUser
  :> "user" :> "login"
  :> Get '[HTML] Html

type HandleUserLoginRoute =
  AuthProtect ReqNoUser
  :> "user" :> "login"
  :> ReqBody '[FormUrlEncoded] (FormValidation LoginData)
  :> Post '[HTML] (Headers '[ Header "Set-Cookie" Text ] Html)

type HandleUserLogoutRoute =
  AuthProtect (ReqAuthorizedUser '[])
  :> "user" :> "logout"
  :> Post '[HTML] Html

type UserLoginAPI =
       ShowUserLoginRoute
  :<|> HandleUserLoginRoute
  :<|> HandleUserLogoutRoute
