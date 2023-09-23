module Buki.StaticFrontend.User.Registration.API where

import Servant.API
import Text.Blaze.Html5 (Html)

import Buki.StaticFrontend.Core.HtmlContent
import Buki.StaticFrontend.Core.FormValidation
import Buki.StaticFrontend.User.Registration.Types
import Buki.StaticFrontend.Core.Auth

type ShowUserRegistrationRoute = AuthProtect ReqNoUser :> "user" :> "register" :> Get '[HTML] Html
type HandleUserRegistrationRoute = AuthProtect ReqNoUser :> "user" :> "register" :> ReqBody '[FormUrlEncoded] (FormValidation RegisterData) :> Post '[HTML] Html

type UserRegistrationAPI =
       ShowUserRegistrationRoute
  :<|> HandleUserRegistrationRoute
