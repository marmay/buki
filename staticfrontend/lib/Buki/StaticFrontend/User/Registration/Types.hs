module Buki.StaticFrontend.User.Registration.Types (
  RegisterData (..),
  RegistrationError (..),
  registerDataNameName,
  registerDataEmailName,
  registerDataKidsgroupIdName,
  registerDataKidSymbolName,
  registerDataPasswordName,
  registerDataConfirmPasswordName,
) where

import Buki.StaticFrontend.Core.FormValidation (ValidateFromForm (..), formValidate)
import Buki.Types (EmailAddress, Name, Password)
import Data.Text (Text)
import Data.UUID (UUID)

data RegisterData = RegisterData
  { registerDataName :: Name
  , registerDataEmail :: EmailAddress
  , registerDataKidsgroupId :: UUID
  , registerDataKidSymbol :: Name
  , registerDataPassword :: Password
  , registerDataConfirmPassword :: Password
  }
  deriving (Show, Eq)

registerDataNameName, registerDataEmailName, registerDataKidsgroupIdName, registerDataKidSymbolName, registerDataPasswordName, registerDataConfirmPasswordName :: Text
registerDataNameName = "name"
registerDataEmailName = "email"
registerDataKidsgroupIdName = "kidsgroup_id"
registerDataKidSymbolName = "kid_symbol"
registerDataPasswordName = "password"
registerDataConfirmPasswordName = "confirmPassword"

instance ValidateFromForm RegisterData where
  validateFromForm form =
    RegisterData
      <$> formValidate registerDataNameName form
      <*> formValidate registerDataEmailName form
      <*> formValidate registerDataKidsgroupIdName form
      <*> formValidate registerDataKidSymbolName form
      <*> formValidate registerDataPasswordName form
      <*> formValidate registerDataConfirmPasswordName form

data RegistrationError
  = RegistrationDisabledError
  | InputError
  | PasswordsDontMatchError
  | EmailAddressTakenError
  | InternalError Text
  deriving (Show, Eq)
