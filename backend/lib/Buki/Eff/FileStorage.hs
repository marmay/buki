module Buki.Eff.FileStorage (
  fetch,
  store,
  delete,
  collectGarbage,
  runLocalFileStorage,
  FileStorage
) where

import Prelude hiding (writeFile)
import Data.ByteString (ByteString, hGetContents, writeFile)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import System.Directory (copyFile, listDirectory, removeFile)
import System.IO (Handle, IOMode (ReadMode), openFile)

newtype FileId = FileId UUID
  deriving (Eq, Show)

data StorableData
  = FromFile FilePath
  | FromHandle Handle
  | FromBuffer ByteString
  | FromText Text
  deriving (Eq, Show)

data FileStorage :: Effect where
  Fetch :: FileId -> FileStorage m Handle
  Store :: StorableData -> FileStorage m FileId
  Delete :: FileId -> FileStorage m ()
  CollectGarbage :: (FileId -> Bool) -> FileStorage m ()

makeEffect ''FileStorage

runLocalFileStorage :: (IOE :> es) => FilePath -> Eff (FileStorage ': es) a -> Eff es a
runLocalFileStorage basePath =
  interpret $ \_ -> \case
    Fetch fileId -> liftIO $ localFetch basePath fileId
    Store storableData -> liftIO $ localStore basePath storableData
    Delete fileId -> liftIO $ localDelete basePath fileId
    CollectGarbage p -> liftIO $ localCollectGarbage basePath p

storeAt :: FilePath -> StorableData -> IO ()
storeAt path (FromFile filePath) = copyFile filePath path
storeAt path (FromHandle handle) = hGetContents handle >>= writeFile path
storeAt path (FromBuffer buffer) = writeFile path buffer
storeAt path (FromText text) = writeFile path (encodeUtf8 text)

assemblePath :: FilePath -> FileId -> FilePath
assemblePath basePath (FileId uuid) = basePath <> "/" <> show uuid

disassemblePath :: FilePath -> FilePath -> FileId
disassemblePath basePath path =
  FileId $ read $ drop (length basePath + 1) path

localFetch :: FilePath -> FileId -> IO Handle
localFetch basePath fileId = do
  openFile (assemblePath basePath fileId) ReadMode

localStore :: FilePath -> StorableData -> IO FileId
localStore basePath storableData = do
  uuid <- nextRandom
  let fileId = FileId uuid
  let path = assemblePath basePath fileId
  storeAt path storableData
  pure fileId

localDelete :: FilePath -> FileId -> IO ()
localDelete basePath fileId =
  removeFile (assemblePath basePath fileId)

localCollectGarbage :: FilePath -> (FileId -> Bool) -> IO ()
localCollectGarbage basePath p = do
  files <- listDirectory basePath
  let fileIds = map (disassemblePath basePath) files
  let garbage = filter p fileIds
  mapM_ (localDelete basePath) garbage
