module Buki.StaticFrontend.Static.API (StaticAPI) where

import Servant.API

type StaticAPI = "static" :> Raw
