{-# LANGUAGE OverloadedStrings #-}

module Main where

--import Servant
--import Network.Wai.Handler.Warp (run)
--import Buki.StaticFrontend.User.Registration.API
--import Buki.StaticFrontend.User.Registration.Controller
--import Control.Monad.Reader (runReaderT)
--import Buki.StaticFrontend.Core.AppM (AppConfig(..), withAppContext, AppM)
--import Database.PostgreSQL.Simple (ConnectInfo(..))

--appConfig :: AppConfig
--appConfig = AppConfig
--  { appConfigDbConnectInfo = ConnectInfo
--    { connectHost = "localhost"
--    , connectPort = 9000
--    , connectUser = "postgres"
--    , connectPassword = ""
--    , connectDatabase = "buki"
--    }
--  }

main :: IO ()
main = do
  putStrLn "Currently, this is out-of-date, see DevelMain for the version that works"
--  run 8080 $ serve (Proxy @UserRegistrationAPI) $
--    hoistServer (Proxy @UserRegistrationAPI) nt userRegistrationServer
--  where
--    nt :: forall a. AppM a -> Handler a
--    nt f = withAppContext appConfig $ \ctx -> runReaderT f ctx
