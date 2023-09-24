module Buki.StaticFrontend.User.Login.Types where

import Data.Text (Text)

import Buki.StaticFrontend.Core.FormValidation (ValidateFromForm(..), formValidate)
import Buki.Types (EmailAddress, Password)

data LoginData = LoginData
  { loginDataEmail :: EmailAddress
  , loginDataPassword :: Password
  } deriving (Eq, Show)

loginDataEmailName, loginDataPasswordName :: Text
loginDataEmailName = "email"
loginDataPasswordName = "password"

instance ValidateFromForm LoginData where
  validateFromForm form =
    LoginData
      <$> formValidate loginDataEmailName form
      <*> formValidate loginDataPasswordName form

data LoginError
  = LoginErrorInvalidCredentials
  | LoginErrorInvalidInput
  deriving (Eq, Show)
