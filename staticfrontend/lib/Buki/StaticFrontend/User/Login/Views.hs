module Buki.StaticFrontend.User.Login.Views where

import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA

import Buki.StaticFrontend.Core.ViewM
import Buki.StaticFrontend.Core.FormValidation
import Buki.StaticFrontend.Core.Views.Forms
import Buki.StaticFrontend.Core.Views.Message
import Buki.StaticFrontend.Core.Views.RegularPage

import Data.Default (def)
import Data.Proxy (Proxy(..))

import Buki.StaticFrontend.User.Login.API
import Buki.StaticFrontend.User.Login.Types

loginPage :: Maybe LoginError -> FormValidationData -> ViewM H.Html
loginPage loginError formValidationData = do
  form <- loginForm formValidationData
  regularPage
    def
      { regularPageSubTitle = Just "Login"
      , regularPageMessages = toMessages loginError
      }
    form

loginSucceededPage :: ViewM H.Html
loginSucceededPage = regularPage def {regularPageSubTitle = Just "Login erfolgreich"} $ do
  H.p "Sie sind nun eingeloggt."

logoutSucceededPage :: Bool -> ViewM H.Html
logoutSucceededPage sessionDestroyed =
  regularPage def {regularPageSubTitle = Just "Logout erfolgreich"} $ do
    H.p "Sie sind nun ausgeloggt."
    if sessionDestroyed
      then H.p "Ihre Sitzung wurde beendet."
      else H.p "Es wurde keine aktive Sitzung für Ihren Nutzer gefunden."

toMessages :: Maybe LoginError -> [Message]
toMessages Nothing = []
toMessages (Just LoginErrorInvalidCredentials) =
  [ Message ErrorMessage "Benutzername oder Passwort falsch!"
        "Der Benutzername oder das Passwort ist falsch. Bitte versuchen Sie es erneut. Sollten Sie Ihr Passwort vergessen haben, so können Sie ein neues anfordern, indem Sie eine E-Mail an den Administrator schicken."
  ]
toMessages (Just LoginErrorInvalidInput) =
  [ Message ErrorMessage "Login fehlgeschlagen!"
        "Bitte kontrollieren Sie die Eingaben und versuchen Sie es erneut."
  ]

loginForm :: FormValidationData -> ViewM H.Html
loginForm formValidationData = do
  actionPath <- mkLink (Proxy @UserLoginAPI) (Proxy @HandleUserLoginRoute)
  pure $ H.form H.! HA.action (H.toValue actionPath) H.! HA.method "post" $ do
    textField loginDataEmailName "E-Mail" "erika.musterfrau@example.com" formValidationData
    passwordField loginDataPasswordName "Passwort" formValidationData
    H.button H.! HA.type_ "submit" H.! HA.class_ "btn btn-primary" $ "Login"
