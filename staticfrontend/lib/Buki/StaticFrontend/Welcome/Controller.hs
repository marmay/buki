module Buki.StaticFrontend.Welcome.Controller where

import Buki.StaticFrontend.Core.AppM
import Buki.StaticFrontend.Welcome.Views (welcomePage, termsOfUsePage)
import Buki.Backend.Auth (AuthorizedUser)
import Text.Blaze.Html5 (Html)
import Servant.API
import Servant.Server
import Buki.StaticFrontend.Welcome.API

showWelcomePage :: Maybe AuthorizedUser -> AppM Html
showWelcomePage authorizedUser = liftViewM authorizedUser $ welcomePage []

showTermsOfUsePage :: Maybe AuthorizedUser -> AppM Html
showTermsOfUsePage authorizedUser = liftViewM authorizedUser termsOfUsePage

showPrivacyPolicyPage :: Maybe AuthorizedUser -> AppM Html
showPrivacyPolicyPage authorizedUser = liftViewM authorizedUser $ welcomePage []

showImprintPage :: Maybe AuthorizedUser -> AppM Html
showImprintPage authorizedUser = liftViewM authorizedUser $ welcomePage []

welcomeServer :: ServerT WelcomeAPI AppM
welcomeServer =
       showWelcomePage
  :<|> showWelcomePage
  :<|> showTermsOfUsePage
  :<|> showPrivacyPolicyPage
  :<|> showImprintPage

