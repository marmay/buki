{-# LANGUAGE RecordWildCards #-}

module Buki.StaticFrontend.Core.Views.Menu where

import Buki.Backend.Auth (AuthorizedUser (..))
import Buki.StaticFrontend.Core.ViewM

import qualified Text.Blaze.Html5 as H

import Buki.StaticFrontend.User.Registration.API (UserRegistrationAPI, ShowUserRegistrationRoute)
import Data.Proxy

menuBar :: Maybe AuthorizedUser -> ViewM H.Html
menuBar (Just AuthorizedUser{..}) = pure $ do
  H.div $ do
    H.p $ do
      H.toHtml (H.text "Logged in as")
menuBar Nothing = do
  registerLink <- mkLink (Proxy @UserRegistrationAPI) (Proxy @ShowUserRegistrationRoute)
  pure $ H.div $ (H.text $ "Not logged in: " <> registerLink)
