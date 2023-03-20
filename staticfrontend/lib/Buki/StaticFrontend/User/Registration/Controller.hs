{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE RecordWildCards #-}
module Buki.StaticFrontend.User.Registration.Controller where

import Buki.StaticFrontend.Core.HtmlContent
import Servant.API
import Text.Blaze.Html5 (Html)

import Buki.StaticFrontend.User.Registration.Types
import Buki.StaticFrontend.User.Registration.Views
import Data.Default
import Data.Proxy
import Servant.Server
import Buki.StaticFrontend.Core.AppM
import Buki.StaticFrontend.Core.FormValidation

import qualified Buki.Eff.Db as E
import qualified Buki.Backend.User as B
import Buki.Model.Types (Id(..), toUuid)
import Buki.Validation (unvalidate)

import Control.Monad.Reader (asks)
import Data.UUID (nil, toText)
import Data.Text (Text)
import Buki.Err (Err(..), runUnionHandler, UnionHandler(..))
import Effectful (runEff, liftIO)
import qualified Buki.Union
import qualified Buki.Backend.Kidsgroup as BK
import Buki.Model.Types.User (UserId)
import Buki.StaticFrontend.Core.Views.Message
import Buki.StaticFrontend.Core.Views.RegularPage (RegularPage(..), regularPage)

type ShowUserRegistrationRoute = "user" :> "register" :> Get '[HTML] Html
type HandleUserRegistrationRoute = "user" :> "register" :> ReqBody '[FormUrlEncoded] (FormValidation RegistrationData) :> Post '[HTML] Html

type UserRegistrationAPI =
  ShowUserRegistrationRoute
    :<|> HandleUserRegistrationRoute

type LoginAPI = "login" :> Get '[HTML] Html

type AllAPI = UserRegistrationAPI :<|> LoginAPI

showUserRegistration' :: [Message] -> FormValidationData -> AppM Html
showUserRegistration' overallMessages formData = do
  conn <- asks appDbConn
  actionPath <- safeLink' (Proxy @UserRegistrationAPI) (Proxy @HandleUserRegistrationRoute)
  kidsgroups <- liftIO $ runEff $ E.runDb conn $ BK.runKidsgroupDb $ do
    BK.listKidsgroups
  case kidsgroups of
    [] ->
      pure registrationDisabledPage
    kidsgroups' ->
      pure $ regularPage (def { regularPageMessages = overallMessages })
           $ registrationForm actionPath Nothing (map (\BK.ListKidsgroup{..} -> (toText (toUuid listKidsgroupId), unvalidate listKidsgroupName)) kidsgroups') formData

failUserRegistration :: Text -> FormValidationData -> AppM Html
failUserRegistration errorText = showUserRegistration' [Message ErrorMessage "Registrierung fehlgeschlagen!" errorText]

showUserRegistration :: AppM Html
showUserRegistration = showUserRegistration' [] mempty

handleUserRegistration :: FormValidation RegistrationData -> AppM Html
handleUserRegistration (FormValidation formData Nothing) =
  failUserRegistration "Einige der eingegebenen Daten sind fehlerhaft!" formData
handleUserRegistration (FormValidation formData (Just registrationData))
  | registrationDataPassword registrationData /= registrationDataConfirmPassword registrationData =
    failUserRegistration "Die Passwörter stimmen nicht überein!" formData
  | otherwise = do
      conn <- asks appDbConn
      r <- liftIO $ runEff $ E.runDb conn $ B.runUserDb $ do
        B.registerUser $ B.RegisterData
          { B.registerDataName = registrationDataName registrationData
          , B.registerDataEmail = registrationDataEmail registrationData
          , B.registerDataPassword = registrationDataPassword registrationData
          , B.registerDataKidsgroup = Id nil
          , B.registerDataKidsymbol = ""
          }
      makeResponse r
   where makeResponse :: Buki.Err.Err '[B.EmailAddressTakenError, B.InvalidKidsgroupIdError] UserId -> AppM Html
         makeResponse (Failure f) = failUserRegistration (toErrorMessage f) formData
         makeResponse (Success _) = showUserRegistration' [Message SuccessMessage "Registrierung erfolgreich!" "Ihr Benutzer wurde angelegt. Nach einer manuellen Überprüfung schalten wir Ihren Benutzer frei und senden Ihnen eine E-Mail."] formData

toErrorMessage :: Buki.Union.Union '[B.EmailAddressTakenError, B.InvalidKidsgroupIdError] -> Text
toErrorMessage =
  runUnionHandler $
                 UnionHandler (\(_ :: B.EmailAddressTakenError)  -> "Email address already taken")
             ::: UnionHandler (\(_ :: B.InvalidKidsgroupIdError) -> "Invalid kidsgroup id")


userRegistrationServer :: ServerT UserRegistrationAPI AppM
userRegistrationServer = showUserRegistration :<|> handleUserRegistration
