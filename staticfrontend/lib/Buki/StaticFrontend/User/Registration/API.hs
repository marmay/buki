module Buki.StaticFrontend.User.Registration.API where

import Buki.StaticFrontend.Core.Preludes.API
import Buki.StaticFrontend.User.Registration.Types

type ShowUserRegistrationRoute =
  AuthProtect ReqNoUser
  :> "user" :> "register"
  :> Get '[HTML] Html
  
type HandleUserRegistrationRoute =
  AuthProtect ReqNoUser
  :> "user" :> "register"
  :> ReqBody '[FormUrlEncoded] (FormValidation RegisterData)
  :> Post '[HTML] Html

type UserRegistrationAPI =
       ShowUserRegistrationRoute
  :<|> HandleUserRegistrationRoute
