module DevelMain (update) where

import Servant
import Network.Wai.Handler.Warp (run)
import Buki.StaticFrontend.User.Registration.Controller
import Control.Monad.Reader (runReaderT)
import Buki.StaticFrontend.Core.AppM (AppConfig(..), withAppContext, AppM)
import Database.PostgreSQL.Simple (ConnectInfo(..))

import Rapid

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

main :: IO ()
main = do
  putStrLn "Starting server on port 8080"
  run 8080 $ serve (Proxy @UserRegistrationAPI) $
    hoistServer (Proxy @UserRegistrationAPI) nt userRegistrationServer
  where
    nt :: forall a. AppM a -> Handler a
    nt f = withAppContext appConfig $ \ctx -> runReaderT f ctx

update :: IO (Rapid String)
update =
  rapid 0 $ \r -> do
    restart r "webserver" main
    return r
