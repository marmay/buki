{-# LANGUAGE RecordWildCards #-}
module Buki.StaticFrontend.Core.AppM where

import Data.Text (Text)
import Data.Proxy
import Servant
import Database.PostgreSQL.Simple (Connection, ConnectInfo, connect, close)
import Control.Monad.Catch (bracket, MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Buki.StaticFrontend.Core.ViewM (ViewM, ViewContext(..))
import Control.Monad.Reader (ReaderT, asks, runReader, runReaderT)
import Buki.Backend.Auth (AuthorizedUser)

import Effectful (Eff, IOE, runEff)
import qualified Buki.Eff.Db as E (Db, runDb)
import qualified Buki.Eff.Time as E (Time, runTime)
import qualified Buki.Eff.FileStorage as E (FileStorage, runLocalFileStorage)

data AppConfig = AppConfig
  { appConfigDbConnectInfo :: ConnectInfo
  , appConfigFileStorageDir :: FilePath
  }

data AppContext = AppContext
  { appUrlPrefix :: Text
  , appDbConn :: Connection
  , appFileStorageDir :: FilePath
  }

toViewContext :: Maybe AuthorizedUser -> AppContext -> ViewContext
toViewContext authorizedUser AppContext{..} = ViewContext
  { viewContextUrlPrefix = appUrlPrefix
  , viewContextAuthorizedUser = authorizedUser
  }

type AppM = ReaderT AppContext Handler

runAppM :: AppContext -> AppM a -> Handler a
runAppM = flip runReaderT

withAppContext :: forall m a. (MonadIO m, MonadMask m) => AppConfig -> (AppContext -> m a) -> m a
withAppContext conf =
  bracket (mkAppContext conf) closeAppContext

mkAppContext :: forall m. (MonadIO m) => AppConfig -> m AppContext
mkAppContext AppConfig{..} = do
  appDbConn' <- liftIO $ connect appConfigDbConnectInfo

  pure $ AppContext
    { appUrlPrefix = "http://localhost:8080/"
    , appDbConn = appDbConn'
    , appFileStorageDir = appConfigFileStorageDir
    }

closeAppContext :: forall m. (MonadIO m) => AppContext -> m ()
closeAppContext AppContext{..} = do
  liftIO $ close appDbConn

safeLink' :: forall endpoint api. (IsElem endpoint api, HasLink endpoint, ToHttpApiData (MkLink endpoint Link)) => Proxy api -> Proxy endpoint -> AppM Text
safeLink' api endpoint = do
  urlPrefix' <- asks appUrlPrefix
  pure $ urlPrefix' <> toUrlPiece (safeLink api endpoint)

runEffects :: forall a. Eff '[E.FileStorage, E.Db, E.Time, IOE] a -> AppM a
runEffects eff = do
  conn <- asks appDbConn
  localFileStorageDir <- asks appFileStorageDir
  liftIO
    $ runEff
    $ E.runTime
    $ E.runDb conn
    $ E.runLocalFileStorage localFileStorageDir
    $ eff

liftViewM :: Maybe AuthorizedUser -> ViewM a -> AppM a
liftViewM user a = asks (runReader a . toViewContext user)
