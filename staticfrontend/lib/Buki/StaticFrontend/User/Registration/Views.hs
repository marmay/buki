module Buki.StaticFrontend.User.Registration.Views
  ( registrationForm
  , registrationDisabledPage
  ) where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Buki.StaticFrontend.User.Registration.Types
import Data.Text (Text)
import Buki.StaticFrontend.Core.FormValidation
import Buki.StaticFrontend.Core.Views.Forms

errorMessage :: Maybe Text -> H.Html
errorMessage Nothing = mempty
errorMessage (Just msg) = H.div H.! HA.class_ "alert alert-danger" $ H.toHtml msg

form :: Text -> H.Html -> H.Html
form actionPath inner = H.form H.! HA.action (H.toValue actionPath) H.! HA.method "post" $ do
  inner
  H.button H.! HA.type_ "submit" H.! HA.class_ "btn btn-primary" $ "Registrieren"

registrationForm :: Text -> Maybe Text -> [(Text, Text)] -> FormValidationData -> H.Html
registrationForm actionPath msg kidsgroups formValidationData = do
  H.h1 "Registrierung"
  errorMessage msg
  form actionPath $ do
    textField registrationDataNameName "Name" "Max Mustermann" formValidationData
    textField registrationDataEmailName "E-Mail" "erika.musterfrau@example.com" formValidationData
    radioSelectField registrationDataKidsgroupIdName "Gruppe" kidsgroups formValidationData
    textField registrationDataKidSymbolName "Symbol des Kindes" "Apfel" formValidationData
    passwordField registrationDataPasswordName "Passwort" formValidationData
    passwordField registrationDataConfirmPasswordName "Passwort bestätigen" formValidationData

registrationDisabledPage :: H.Html
registrationDisabledPage = do
  H.h1 "Registrierung deaktiviert"
  H.p "Die Registrierung ist derzeit deaktiviert, da das System noch nicht vollständig eingerichtet ist."
