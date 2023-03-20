module Buki.StaticFrontend.Core.HtmlContent where

import Servant.API
import Network.HTTP.Media ((//), (/:))
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Html where
  mimeRender _ = renderHtml
