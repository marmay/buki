{-# LANGUAGE OverloadedStrings #-}

module Buki.StaticFrontend.User.Registration.Views (
  registerDisabledPage,
  registerSucceedPage,
  registerPage,
) where

import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA

import Buki.StaticFrontend.Core.ViewM
import Buki.StaticFrontend.Core.FormValidation
import Buki.StaticFrontend.Core.Views.Forms
import Buki.StaticFrontend.Core.Views.Message
import Buki.StaticFrontend.Core.Views.RegularPage
import Buki.StaticFrontend.User.Registration.Types
import Data.Default (def)
import Data.Text (Text)
import Buki.StaticFrontend.User.Registration.API
import Data.Proxy (Proxy(..))

registerPage' :: [Message] -> H.Html -> H.Html
registerPage' messages =
  regularPage
    def
      { regularPageSubTitle = Just "Registrierung"
      , regularPageMessages = messages
      }

registerPage :: Maybe RegistrationError -> [(Text, Text)] -> FormValidationData -> ViewM H.Html
registerPage registrationError kidsgroups formValidationData = do
  registerPage' (toMessages registrationError) <$> registerForm kidsgroups formValidationData

registerSucceedPage :: H.Html
registerSucceedPage = registerPage' [Message SuccessMessage "Registrierung erfolgreich" "Die Registrierung war erfolgreich. Sie können sich nun einloggen."] mempty

registerDisabledPage :: H.Html
registerDisabledPage =
  registerPage'
    [Message ErrorMessage "Registrierung deaktiviert" "Die Registrierung ist derzeit deaktiviert, da das System noch nicht vollständig eingerichtet ist."]
    mempty

registerForm :: [(Text, Text)] -> FormValidationData -> ViewM H.Html
registerForm kidsgroups formValidationData = do
  actionPath <- mkLink (Proxy @UserRegistrationAPI) (Proxy @HandleUserRegistrationRoute)
  pure $ H.form H.! HA.action (H.toValue actionPath) H.! HA.method "post" $ do
    textField registerDataNameName "Name" "Max Mustermann" formValidationData
    textField registerDataEmailName "E-Mail" "erika.musterfrau@example.com" formValidationData
    radioSelectField registerDataKidsgroupIdName "Gruppe" kidsgroups formValidationData
    textField registerDataKidSymbolName "Symbol des Kindes" "Apfel" formValidationData
    passwordField registerDataPasswordName "Passwort" formValidationData
    passwordField registerDataConfirmPasswordName "Passwort bestätigen" formValidationData
    H.button H.! HA.type_ "submit" H.! HA.class_ "btn btn-primary" $ "Registrieren"

toMessages :: Maybe RegistrationError -> [Message]
toMessages Nothing = []
toMessages (Just RegistrationDisabledError) =
  [ Message ErrorMessage "Registrierung deaktiviert!"
      "Die Registrierung ist derzeit deaktiviert, da das System noch nicht vollständig eingerichtet ist."
  ]
toMessages (Just InputError) =
  [ Message ErrorMessage "Registrierung fehlgeschlagen!"
      "Bitte kontrollieren Sie die Eingaben und versuchen Sie es erneut."
  ]
toMessages (Just PasswordsDontMatchError) =
  [ Message ErrorMessage "Registrierung fehlgeschlagen!"
        "Die Passwörter stimmen nicht überein."
  ]
toMessages (Just EmailAddressTakenError) =
  [ Message ErrorMessage "Registrierung fehlgeschlagen!"
          "Die E-Mail-Adresse ist bereits vergeben."
  ]
toMessages (Just (InternalError errorCode)) =
  [ Message ErrorMessage "Registrierung fehlgeschlagen!"
          ("Es ist ein interner Fehler aufgetreten. Bitte versuchen Sie es später erneut." <>
           "Sollte der Fehler weiterhin auftreten, so melden Sie sich bitte beim Administrator." <> "Fehlercode: " <> errorCode)
  ]
