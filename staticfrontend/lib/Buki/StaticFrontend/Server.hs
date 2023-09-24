module Buki.StaticFrontend.Server where

import Data.Proxy (Proxy(..))
import Servant.API
import Servant.Server
import Buki.StaticFrontend.Core.AppM

import Buki.StaticFrontend.User.Server
import Buki.StaticFrontend.Static.API
import Buki.StaticFrontend.Static.Server

type ServerAPI = (
         UserAPI
    :<|> StaticAPI
    )

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

server :: ServerT ServerAPI AppM
server = userServer :<|> staticServer
