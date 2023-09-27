module Buki.StaticFrontend.Welcome.API where

import Buki.StaticFrontend.Core.Preludes.API

type WelcomeRoute =
  AuthProtect ReqOptionalUser
  :> "welcome"
  :> Get '[HTML] Html

type TermsOfUseRoute =
  AuthProtect ReqOptionalUser
  :> "terms-of-use"
  :> Get '[HTML] Html

type PrivacyPolicyRoute =
  AuthProtect ReqOptionalUser
  :> "privacy-policy"
  :> Get '[HTML] Html

type ImprintRoute =
  AuthProtect ReqOptionalUser
  :> "imprint"
  :> Get '[HTML] Html
  
type IndexRoute =
  AuthProtect ReqOptionalUser
  :> Get '[HTML] Html

type WelcomeAPI =
       WelcomeRoute
  :<|> IndexRoute
  :<|> TermsOfUseRoute
  :<|> PrivacyPolicyRoute
  :<|> ImprintRoute
