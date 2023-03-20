{-# LANGUAGE RecordWildCards #-}

module Buki.StaticFrontend.Core.Views.Message
  ( renderMessages
  , renderMessage
  , reorderMessages
  , Message(..)
  , MessageType(..)
  ) where

import Data.Text (Text)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html5 ((!))
import Data.List (sortOn)
import Data.Foldable (for_)

data MessageType
  = ErrorMessage
  | SuccessMessage
  | WarningMessage
  | InfoMessage
  deriving (Eq, Show, Ord, Bounded)

data Message = Message
  { messageType :: MessageType
  , messageTitle :: Text
  , messageText :: Text
  } deriving (Eq, Show)

messageTypeClass :: MessageType -> Text
messageTypeClass ErrorMessage = "msg_t_error"
messageTypeClass SuccessMessage = "msg_t_success"
messageTypeClass WarningMessage = "msg_t_warning"
messageTypeClass InfoMessage = "msg_t_info"

reorderMessages :: [Message] -> [Message]
reorderMessages = sortOn messageType

renderMessage :: Message -> H.Html
renderMessage Message{..} = do
  H.div ! HA.class_ (H.toValue ("msg " <> messageTypeClass messageType)) $ do
    H.div ! HA.class_ "msg_title" $ H.toHtml messageTitle
    H.div ! HA.class_ "msg_text" $ H.toHtml messageText

renderMessages :: [Message] -> H.Html
renderMessages messages = for_ (reorderMessages messages) renderMessage
