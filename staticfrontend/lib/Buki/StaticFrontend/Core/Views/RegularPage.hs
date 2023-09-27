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
import Data.Maybe (fromMaybe)

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
  u <- askAuthorizedUser
  menuBar' <- menuBar u
  bottomBar' <- bottomBar
  pure $ H.html $ do
    H.head $ do
      H.title $ H.toHtml fullTitle
      H.link
        ! HA.rel "stylesheet"
        ! HA.href "/static/css/bootstrap.min.css"
      H.link
        ! HA.rel "stylesheet"
        ! HA.href "/static/css/buki.css"
    H.body $ do
      H.div ! HA.class_ "background" $ do
        H.div ! HA.class_ "content container py-2" $ do
          menuBar'
          H.div ! HA.class_ "mx-auto px-xs-1 px-sm-3 px-md-5 py-4 inner_content" $ do
            H.h1 $ H.toHtml header
            renderMessages regularPageMessages
            inner
          bottomBar'
  where
    fullTitle = case regularPageSubTitle of
      Nothing -> regularPageTitle
      Just subTitle -> regularPageTitle <> " - " <> subTitle
    header = fromMaybe regularPageTitle regularPageSubTitle
