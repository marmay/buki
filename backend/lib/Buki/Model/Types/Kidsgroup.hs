module Buki.Model.Types.Kidsgroup
  ( Kidsgroup'(..)
  , Kidsgroup
  , KidsgroupField
  , KidsgroupId
  , pKidsgroup
  ) where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance')

import Buki.Model.TH
import Buki.Model.Types.Id
import Buki.Types

data Kidsgroup' t1 t2 = Kidsgroup
  { kidsgroup'Id :: t1
  -- ^ Primary key
  , kidsgroup'Name :: t2
  -- ^ Unique name of the group
  }

type KidsgroupId = Id Kidsgroup'
makeDbAliases ''Kidsgroup' [ [t|KidsgroupId|]
                           , [t|Name|]
                           ]
makeAdaptorAndInstance' ''Kidsgroup'
