{-# LANGUAGE UndecidableInstances #-}

module Buki.Model.Tag
  ( Tag'(..)
  , Tag
  , TagId
  , TagField
  , pTag
  , tagTable
  ) where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance')
import Data.Text (Text)

import Buki.Model.Util.SqlType (SqlType)
import Buki.Model.Util.TH
import Buki.Model.Id

data Tag' t1 t2 = Tag
  { tag'Id :: t1
  , tag'Name :: t2
  }
type TagId = Id Tag'
makeDbAliases ''Tag' [ [t|TagId|]
                     , [t|Text|]
                     ]
makeAdaptorAndInstance' ''Tag'
makeDbTable "tags" ''Tag
