{-# LANGUAGE RecordWildCards #-}

module Buki.StaticFrontend.User.Registration.Controller where

import Text.Blaze.Html5 (Html)

import Buki.StaticFrontend.Core.AppM
import Buki.StaticFrontend.Core.FormValidation
import Buki.StaticFrontend.User.Registration.API
import Buki.StaticFrontend.User.Registration.Types
import Buki.StaticFrontend.User.Registration.Views
import Servant.API
import Servant.Server

import Buki.Backend.User qualified as BU
import Buki.Model (Id (..), toUuid)
import Buki.Validation (unvalidate)

import Buki.Backend.Kidsgroup qualified as BK
import Buki.Err (Err (..), UnionHandler (..), runUnionHandler)
import Data.UUID (toText)

-- | Displays the user registration page.
showUserRegister :: () -> AppM Html
showUserRegister _ = showUserRegister' Nothing mempty

-- | Displays the user registration page with an error message.
failUserRegister :: RegistrationError -> FormValidationData -> AppM Html
failUserRegister = showUserRegister' . Just

-- | Displays the user registration page; used by showUserRegister and failUserRegister.
showUserRegister' :: Maybe RegistrationError -> FormValidationData -> AppM Html
showUserRegister' registrationError formData = do
  kidsgroups <- fmap toKeyNamePair <$> runEffects BK.listKidsgroups
  case kidsgroups of
    [] -> liftViewM Nothing registerDisabledPage
    kidsgroups' ->
      liftViewM Nothing $ registerPage registrationError kidsgroups' formData
 where
  toKeyNamePair BK.ListKidsgroup{..} = (toText (toUuid listKidsgroupId), unvalidate listKidsgroupName)

handleUserRegister :: () -> FormValidation RegisterData -> AppM Html
handleUserRegister _ (FormValidation formData Nothing) =
  failUserRegister InputError formData
handleUserRegister _ (FormValidation formData (Just registerData))
  | registerDataPassword registerData /= registerDataConfirmPassword registerData =
      failUserRegister PasswordsDontMatchError formData
  | otherwise = tryRegistration >>= makeResponse
 where
  tryRegistration =
    runEffects
      $ BU.registerUser
      $ BU.RegisterData
        { BU.registerDataName = registerDataName registerData
        , BU.registerDataEmail = registerDataEmail registerData
        , BU.registerDataPassword = registerDataPassword registerData
        , BU.registerDataKidsgroup = Id $ registerDataKidsgroupId registerData
        , BU.registerDataKidsymbol = unvalidate $ registerDataKidSymbol registerData
        }

  makeResponse (Success _) = liftViewM Nothing registerSucceedPage
  makeResponse (Failure f) = failUserRegister (translateError f) formData

  translateError =
    runUnionHandler
      $ UnionHandler (\(_ :: BU.EmailAddressTakenError) -> EmailAddressTakenError)
      ::: UnionHandler (\(_ :: BU.InvalidKidsgroupIdError) -> InternalError "INVALID_KIDSGROUP_ID")

userRegistrationServer :: ServerT UserRegistrationAPI AppM
userRegistrationServer = showUserRegister :<|> handleUserRegister
