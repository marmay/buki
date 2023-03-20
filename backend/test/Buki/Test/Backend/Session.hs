module Buki.Test.Backend.Session where

import Buki.Backend.Auth
import Buki.Backend.User
import Buki.Backend.Session
import Buki.Types
import Buki.Validation

import Data.Text

userName :: Name
userName = forceValidate @Text "Test User"

password :: Password
password = forceValidate @Text "testtest"

email :: EmailAddress
email    = forceValidate @Text "test@test"
