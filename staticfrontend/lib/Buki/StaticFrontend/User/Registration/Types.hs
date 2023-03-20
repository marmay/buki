module Buki.StaticFrontend.User.Registration.Types
  ( RegistrationData(..)
  , registrationDataNameName
  , registrationDataEmailName
  , registrationDataKidsgroupIdName
  , registrationDataKidSymbolName
  , registrationDataPasswordName
  , registrationDataConfirmPasswordName
  ) where

import Data.Text (Text)
import Buki.Types (Name, EmailAddress, Password)
import Buki.StaticFrontend.Core.FormValidation (ValidateFromForm (..), formValidate)
import Data.UUID (UUID)

data RegistrationData = RegistrationData
  { registrationDataName :: Name
  , registrationDataEmail :: EmailAddress
  , registrationDataKidsgroupId :: UUID
  , registrationDataKidSymbol :: Name
  , registrationDataPassword :: Password
  , registrationDataConfirmPassword :: Password
  } deriving (Show, Eq)

registrationDataNameName, registrationDataEmailName, registrationDataKidsgroupIdName, registrationDataKidSymbolName, registrationDataPasswordName, registrationDataConfirmPasswordName :: Text
registrationDataNameName = "name"
registrationDataEmailName = "email"
registrationDataKidsgroupIdName = "kidsgroup_id"
registrationDataKidSymbolName = "kid_symbol"
registrationDataPasswordName = "password"
registrationDataConfirmPasswordName = "confirmPassword"

instance ValidateFromForm RegistrationData where
  validateFromForm form = RegistrationData
    <$> formValidate registrationDataNameName form
    <*> formValidate registrationDataEmailName form
    <*> formValidate registrationDataKidsgroupIdName form
    <*> formValidate registrationDataKidSymbolName form
    <*> formValidate registrationDataPasswordName form
    <*> formValidate registrationDataConfirmPasswordName form
