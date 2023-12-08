module Buki.Model.Types.Place
  ( Place'(..)
  , Place
  , PlaceId
  , PlaceField
  , pPlace
  ) where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance')
import Data.Text (Text)

import Buki.Model.SqlType (SqlType)
import Buki.Model.TH
import Buki.Model.Types.Id

-- | A place where books can be located.
data Place' t1 t2 = Place
  { place'Id :: t1
  -- ^ Primary key

  , place'Name :: t2
  -- ^ Name of the place.
  } deriving (Eq, Show)
type PlaceId = Id Place'
makeDbAliases ''Place' [ [t|PlaceId|]
                       , [t|Text|]
                       ]
makeAdaptorAndInstance' ''Place'
