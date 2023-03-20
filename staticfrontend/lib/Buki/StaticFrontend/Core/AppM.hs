{-# LANGUAGE RecordWildCards #-}
module Buki.StaticFrontend.Core.AppM where

import Data.Text (Text)
import Control.Monad.Reader (ReaderT, asks)
import Data.Proxy
import Servant
import Database.PostgreSQL.Simple (Connection, ConnectInfo, connect, close)
import Control.Monad.Catch (bracket, MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)

newtype AppConfig = AppConfig
  { appConfigDbConnectInfo :: ConnectInfo
  }

data AppContext = AppContext
  { appUrlPrefix :: Text
  , appDbConn :: Connection
  }

type AppM = ReaderT AppContext Handler

withAppContext :: forall m a. (MonadIO m, MonadMask m) => AppConfig -> (AppContext -> m a) -> m a
withAppContext conf =
  bracket (mkAppContext conf) closeAppContext

mkAppContext :: forall m. (MonadIO m) => AppConfig -> m AppContext
mkAppContext AppConfig{..} = do
  appDbConn' <- liftIO $ connect appConfigDbConnectInfo

  pure $ AppContext
    { appUrlPrefix = "http://localhost:8080/"
    , appDbConn = appDbConn'
    }

closeAppContext :: forall m. (MonadIO m) => AppContext -> m ()
closeAppContext AppContext{..} = do
  liftIO $ close appDbConn

safeLink' :: forall endpoint api. (IsElem endpoint api, HasLink endpoint, ToHttpApiData (MkLink endpoint Link)) => Proxy api -> Proxy endpoint -> ReaderT AppContext Handler Text
safeLink' api endpoint = do
  urlPrefix' <- asks appUrlPrefix
  pure $ urlPrefix' <> toUrlPiece (safeLink api endpoint)
