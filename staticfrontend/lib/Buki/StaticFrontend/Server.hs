module Buki.StaticFrontend.Server where

import Buki.StaticFrontend.User.Registration.API
import Buki.StaticFrontend.User.Registration.Controller
import Data.Proxy (Proxy(..))
import Servant (HasServer (..))
import Buki.StaticFrontend.Core.AppM

type ServerAPI = UserRegistrationAPI

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

server :: ServerT ServerAPI AppM
server = userRegistrationServer
