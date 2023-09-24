module Buki.StaticFrontend.User.Server where

import Servant.API
import Servant.Server

import Data.Proxy (Proxy(..))
import Buki.StaticFrontend.Core.AppM (AppM)

import Buki.StaticFrontend.User.Registration.API
import Buki.StaticFrontend.User.Registration.Controller
import Buki.StaticFrontend.User.Login.API
import Buki.StaticFrontend.User.Login.Controller

type UserAPI = (
         UserRegistrationAPI
    :<|> UserLoginAPI
    )

userAPI :: Proxy UserAPI
userAPI = Proxy

userServer :: ServerT UserAPI AppM
userServer = userRegistrationServer :<|> userLoginServer
