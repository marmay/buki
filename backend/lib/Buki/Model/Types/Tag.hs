module Buki.Model.Types.Tag
  ( Tag'(..)
  , Tag
  , TagId
  , TagField
  , pTag
  ) where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance')
import Data.Text (Text)

import Buki.Model.TH
import Buki.Model.Types.Id

data Tag' t1 t2 = Tag
  { tag'Id :: t1
  , tag'Name :: t2
  }
type TagId = Id Tag'
makeDbAliases ''Tag' [ [t|TagId|]
                     , [t|Text|]
                     ]
makeAdaptorAndInstance' ''Tag'

