{-# LANGUAGE Arrows #-}

module Buki.Backend.Place where

import Data.Text (Text)

import Effectful

import Buki.Backend.Auth
import Buki.Eff.Db
import Buki.Err

import Control.Lens ((^.))
import Opaleye.Operators ((.==))

import qualified Buki.Model as M
import qualified Opaleye as O

data PlaceData = ListPlaceData
  { placeId :: M.PlaceId
  , placeName :: Text
  }

data PlaceNameTaken = PlaceNameTaken
  deriving (Eq, Show)
data PlaceNotFound = PlaceNotFound
  deriving (Eq, Show)
data PlaceStillUsed = PlaceStillUsed
  deriving (Eq, Show)

-- | List all places.
listPlaces :: (Db :> es) => Eff es [PlaceData]
listPlaces = do
  ps <- dbSelect $ proc () -> do
    O.selectTable M.placeTable -< ()
  pure $ fmap toPlaceData ps

-- | Add a new place.
-- If the place name is already taken, returns 'PlaceNameTaken'.
addPlace :: forall auth es. (Db :> es, auth `HasPermissions` '[ 'BookManagement ]) =>
  auth -> Text -> Eff es (Err '[ PlaceNameTaken ] PlaceData)
addPlace _ pName = do
  pId <- dbMkUuid
  p <- dbCatchViolation (onAnyUniqueViolation (mkFailure PlaceNameTaken))
      $ mkSuccess <$> dbInsertOne M.placeTable (O.toFields M.Place
          { M.place'Id = pId
          , M.place'Name = pName
          })
  pure $ toPlaceData <$> p

-- renamePlace :: forall auth es. (Db :> es, auth `HasPermissions` '[ 'BookManagement ]) =>
--   auth -> M.PlaceId -> Text -> Eff es (Err '[ PlaceNotFound, PlaceNameTaken ] PlaceData)
-- renamePlace _ pId pName = do

-- | Remove a place with a given ID.
-- If the place does not exist, returns 'PlaceNotFound'.
removeUnusedPlace :: forall auth es. (Db :> es, auth `HasPermissions` '[ 'BookManagement ]) =>
  auth -> M.PlaceId -> Eff es (Err '[ PlaceNotFound, PlaceStillUsed ] PlaceData)
removeUnusedPlace _ pId = do
  (fmap . fmap) toPlaceData $
    dbCatchViolation (onAnyForeignKeyViolation (mkFailure PlaceStillUsed)) $
      dbDeleteOne M.placeTable (\p -> p ^. M.id .== O.toFields pId)
      >>= liftE @'[ PlaceNotFound, PlaceStillUsed ] (\NoRecords -> pure $ Left PlaceNotFound)

toPlaceData :: M.Place -> PlaceData
toPlaceData p = ListPlaceData
  { placeId = M.place'Id p
  , placeName = M.place'Name p
  }
