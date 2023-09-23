module Buki.StaticFrontend.Static.Server (staticServer) where

import Servant (HasServer(..), serveDirectoryWebApp)
import Buki.StaticFrontend.Core.AppM
import Buki.StaticFrontend.Static.API

staticServer :: ServerT StaticAPI AppM
staticServer = serveDirectoryWebApp "static"
