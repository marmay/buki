{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Buki.TestUtil.Psql where

import Data.String (IsString (..))
import Database.PostgreSQL.Simple
import System.Directory (doesFileExist)
import System.IO (IOMode (..), hGetContents, withFile)
import System.IO.Temp
import System.Process.Typed

import Buki.Eff.Db
import Effectful
import Effectful.Dispatch.Dynamic

import Control.Concurrent (threadDelay)
import Control.Monad.Catch (MonadMask, bracket)

-- This module provides test infrastructure for running tests against a
-- real postgresql database.

-- | Connection information for the test database.
connectInfo :: ConnectInfo
connectInfo =
  ConnectInfo
    { connectHost = "localhost"
    , connectPort = 8999
    , connectUser = "postgres"
    , connectPassword = ""
    , connectDatabase = "buki"
    }

-- | This is a variant of withConnect from Database.PostgreSQL.Simple, but it
-- | is generalized to use any monad that supports bracket.
withConnection :: (MonadIO m, MonadMask m) => ConnectInfo -> (Connection -> m a) -> m a
withConnection info = bracket (liftIO $ connect info) (liftIO . close)

-- | Given a function that takes a connection, run it against an isolated
-- | test database.
withDatabase :: (MonadIO m, MonadUnliftIO m, MonadMask m) => (Connection -> m a) -> m a
withDatabase fn =
  withSystemTempDirectory "buki-test" $ \dir -> do
    runProcess_ (proc "initdb" ["-U", connectUser connectInfo, dir])
    withProcessTerm (proc "postgres" ["-D", dir, "-k", dir, "-p", show (connectPort connectInfo)]) $ \_ -> do
      -- Wait for postgres to start up. Once it has finished, it will write the lock file
      -- dir/.s.PGSQL.<connectPort>.lock:
      let lockFile = dir <> "/.s.PGSQL." <> show (connectPort connectInfo) <> ".lock"
      waitFile lockFile
      runProcess_ (proc "createdb" ["-h", connectHost connectInfo, "-p", show (connectPort connectInfo), "-U", connectUser connectInfo, connectDatabase connectInfo])
      withConnection connectInfo $ \conn -> do
        fn conn

waitFile :: (MonadIO m, MonadUnliftIO m) => FilePath -> m ()
waitFile path = do
  exists <- liftIO $ doesFileExist path
  if exists
    then pure ()
    else do
      liftIO $ threadDelay 100000
      waitFile path

-- | Run a SQL script against a database connection.
runSqlScript :: FilePath -> Connection -> IO ()
runSqlScript p conn =
  withFile p ReadMode $ \h -> do
    q <- hGetContents h
    _ <- execute_ conn (fromString q)
    pure ()

-- | Interprets the Db effect in terms of a real, temporary postgresql database that
-- | is created and destroyed for each test.
runDbTest :: (HasCallStack, IOE :> es) => Connection -> Eff (Db ': es) a -> Eff es a
runDbTest conn eff = do
  bracket
    (liftIO $ runSqlScript "schema/current.sql" conn)
    (const $ liftIO $ runSqlScript "schema/dropAll.sql" conn)
    (const $ runDb conn eff)
