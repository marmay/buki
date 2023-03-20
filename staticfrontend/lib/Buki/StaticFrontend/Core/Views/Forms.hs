module Buki.StaticFrontend.Core.Views.Forms
  ( passwordField
  , radioSelectField
  , textField
  ) where

import Data.Text (Text)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html5 ((!))
import Data.Maybe (fromMaybe)
import Buki.StaticFrontend.Core.FormValidation (FormValidationData(..), formValidationErrorsOf, formValidationValueOf)

formField :: Text -> Text -> FormValidationData -> (Maybe Text -> H.Html) -> H.Html
formField field label m inner = do
  H.div ! HA.class_ "form-group" $ do
    H.label ! HA.for (H.toValue field) $ H.toHtml label
    inner (formValidationValueOf field m)
    mapM_ (\err -> H.div ! HA.class_ "invalid-feedback" $ H.toHtml err) (formValidationErrorsOf field m)

textField :: Text -> Text -> Text -> FormValidationData -> H.Html
textField field label placeholder m =
  formField field label m $ \value -> do
    H.input
      ! HA.type_ "text"
      ! HA.name (H.toValue field)
      ! HA.class_ "form-control"
      ! HA.placeholder (H.toValue placeholder)
      ! HA.value (H.toValue $ fromMaybe "" value)

passwordField :: Text -> Text -> FormValidationData -> H.Html
passwordField field label m = formField field label m $ \value -> do
  H.input
    ! HA.type_ "password"
    ! HA.name (H.toValue field)
    ! HA.class_ "form-control"
    ! HA.value (H.toValue $ fromMaybe "" value)

radioSelectField :: Text -> Text -> [(Text, Text)] -> FormValidationData -> H.Html
radioSelectField field label options m = formField field label m $ \value -> do
  H.div ! HA.class_ "form-check" $ do
    mapM_ (\(v, l) -> do
      H.input
        ! HA.type_ "radio"
        ! HA.name (H.toValue field)
        ! HA.value (H.toValue v)
        ! HA.class_ "form-check-input"
        ! HA.checked (if value == Just v then "checked" else mempty)
      H.label ! HA.class_ "form-check-label" $ H.toHtml l
      ) options
