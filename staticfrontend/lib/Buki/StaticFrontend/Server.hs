module Buki.StaticFrontend.Server where

import Data.Proxy (Proxy(..))
import Servant.API
import Servant.Server
import Buki.StaticFrontend.Core.AppM

import Buki.StaticFrontend.Static.API
import Buki.StaticFrontend.Static.Server
import Buki.StaticFrontend.User.Registration.API
import Buki.StaticFrontend.User.Registration.Controller

type ServerAPI = (
         UserRegistrationAPI
    :<|> StaticAPI
    )

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

server :: ServerT ServerAPI AppM
server = userRegistrationServer :<|> staticServer
