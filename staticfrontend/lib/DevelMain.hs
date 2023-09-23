module DevelMain (update) where

import Servant
import Network.Wai.Handler.Warp (run)
import Buki.StaticFrontend.Core.AppM (AppConfig(..), withAppContext, AppContext, runAppM)
import Database.PostgreSQL.Simple (ConnectInfo(..))
import Buki.StaticFrontend.Server (serverAPI, server)

import Rapid
import Servant.Server.Experimental.Auth
import Network.Wai (Request)
import Buki.Backend.Auth (Authorization)
import Buki.StaticFrontend.Core.Auth

appConfig :: AppConfig
appConfig = AppConfig
  { appConfigDbConnectInfo = ConnectInfo
    { connectHost = "localhost"
    , connectPort = 9000
    , connectUser = "postgres"
    , connectPassword = ""
    , connectDatabase = "buki"
    }
  }

-- It is a bit of a mess that we have to define every combination of permissions
-- required by the app here. It would be nice, if we would have some support for
-- polymorphism here, but for now, this should work.
type AuthHandlers = '[ AuthHandler Request ()
                     , AuthHandler Request (Maybe (Authorization '[]))
                     ]

genAuthServerCtx :: AppContext -> Context AuthHandlers
genAuthServerCtx ctx =
     authReqNoUser ctx
  :. authReqOptionalUser ctx
  :. EmptyContext

main :: IO ()
main = do
  putStrLn "Starting server on port 8080"
  withAppContext appConfig $ \ctx -> do
    run 8080 $ serveWithContext serverAPI (genAuthServerCtx ctx) $
      hoistServerWithContext serverAPI (Proxy @AuthHandlers) (runAppM ctx) server

update :: IO (Rapid String)
update =
  rapid 0 $ \r -> do
    restart r "webserver" main
    return r