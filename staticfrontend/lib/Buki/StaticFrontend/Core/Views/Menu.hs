{-# LANGUAGE RecordWildCards #-}

module Buki.StaticFrontend.Core.Views.Menu where

import Buki.Backend.Auth (AuthorizedUser (..))
import Buki.StaticFrontend.Core.ViewM

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Data.Proxy
import Buki.StaticFrontend.Welcome.API
import Buki.StaticFrontend.User.Login.API

menuBar :: Maybe AuthorizedUser -> ViewM H.Html
menuBar (Just AuthorizedUser{..}) = do
  l <- leftMenu
  r <- rightMenu
  basicBar l r
  where
    leftMenu = do
      pure $ H.li "Nothing yet"
    rightMenu = do
      logoutLink <- mkLink (Proxy @UserLoginAPI) (Proxy @HandleUserLogoutRoute)
      pure $ H.li $ H.a H.! HA.class_ "nav-link" H.! HA.href (H.toValue logoutLink) $ "Logout"
menuBar Nothing = do
  l <- leftMenu
  r <- rightMenu
  basicBar l r
  where
    leftMenu = do
      pure $ H.li "Nothing yet"
    rightMenu = do
      loginLink <- mkLink (Proxy @UserLoginAPI) (Proxy @ShowUserLoginRoute)
      pure $ H.li $ H.a H.! HA.class_ "nav-link" H.! HA.href (H.toValue loginLink) $ "Login"

basicBar :: H.Html -> H.Html -> ViewM H.Html
basicBar leftMenu rightMenu = do
  welcomeLink <- mkLink (Proxy @WelcomeAPI) (Proxy @WelcomeRoute)
  pure $ H.nav H.! HA.class_ "navbar navbar-expand-lg bd-blue-900" $ do
    H.a H.! HA.class_ "navbar-brand" H.! HA.href (H.toValue welcomeLink) $ "Kindergartenbücherei"
    H.button H.! HA.class_ "navbar-toggler" H.! HA.type_ "button"
      H.! H.dataAttribute "toggle" "collapse"
      H.! H.dataAttribute "target" "#navbarSupportedContent"
      H.! H.customAttribute "aria-expanded" "false"
      H.! H.customAttribute "aria-controls" "navbarSupportedContent"
      H.! H.customAttribute "aria-label" "Toggle navigation" $ do
        H.span H.! HA.class_ "navbar-toggler-icon" $ ""
    H.div H.! HA.id "navbarSupportedContent"
          H.! HA.class_ "collapse navbar-collapse" $ do
      H.ul H.! HA.class_ "navbar-nav mr-auto" $ leftMenu
      H.ul H.! HA.class_ "navbar-nav navbar-right" $ rightMenu
    
bottomBar :: ViewM H.Html
bottomBar = do
  termsOfUseLink <- mkLink (Proxy @WelcomeAPI) (Proxy @TermsOfUseRoute)
  imprintLink <- mkLink (Proxy @WelcomeAPI) (Proxy @ImprintRoute)
  privacyPolicyLink <- mkLink (Proxy @WelcomeAPI) (Proxy @PrivacyPolicyRoute)
  pure $ H.div H.! HA.class_ "bottom_bar py-1 mx-md-5 px-md-5 text-center" $ do
    H.a H.! HA.class_ "mx-2" H.! HA.href (H.toValue termsOfUseLink) $ "Nutzungsbedingungen"
    H.a H.! HA.class_ "mx-2" H.! HA.href (H.toValue imprintLink) $ "Impressum"
    H.a H.! HA.class_ "mx-2" H.! HA.href (H.toValue privacyPolicyLink) $ "Datenschutzerklärung"
