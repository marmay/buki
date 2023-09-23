{-# LANGUAGE RecordWildCards #-}

module Buki.StaticFrontend.Core.Views.RegularPage
  ( regularPage
  , RegularPage(..)
  ) where

import Data.Text (Text)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Buki.StaticFrontend.Core.Views.Message
import Data.Default
import Buki.StaticFrontend.Core.Views.Menu
import Buki.StaticFrontend.Core.ViewM

data RegularPage = RegularPage
  { regularPageTitle :: Text
  , regularPageSubTitle :: Maybe Text
  , regularPageMessages :: [Message]
  }

instance Default RegularPage where
  def = RegularPage
    { regularPageTitle = "Kindergartenbücherei Rohrendorf"
    , regularPageSubTitle = Nothing
    , regularPageMessages = []
    }

regularPage :: RegularPage -> H.Html -> ViewM H.Html
regularPage RegularPage{..} inner = do
  menuBar' <- menuBar Nothing
  pure $ H.html $ do
    H.head $ do
      H.title $ H.toHtml fullTitle
      H.link
        ! HA.rel "stylesheet"
        ! HA.href "/static/css/bootstrap.min.css"
    H.body $ do
      H.h1 "Menu: "
      menuBar'
      H.h1 $ H.toHtml regularPageTitle
      mapM_ (H.h2 . H.toHtml) regularPageSubTitle
      renderMessages regularPageMessages
      inner
  where
    fullTitle = case regularPageSubTitle of
      Nothing -> regularPageTitle
      Just subTitle -> regularPageTitle <> " - " <> subTitle
