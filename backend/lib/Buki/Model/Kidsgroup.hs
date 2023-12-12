{-# LANGUAGE UndecidableInstances #-}

module Buki.Model.Kidsgroup (
    Kidsgroup' (..),
    Kidsgroup,
    KidsgroupField,
    KidsgroupId,
    pKidsgroup,
    kidsgroupTable,
) where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance')

import Buki.Model.Util.SqlType (SqlType)
import Buki.Model.Util.TH
import Buki.Model.Id
import Buki.Types

data Kidsgroup' t1 t2 = Kidsgroup
    { kidsgroup'Id :: t1
    -- ^ Primary key
    , kidsgroup'Name :: t2
    -- ^ Unique name of the group
    }
    deriving (Eq, Show)

type KidsgroupId = Id Kidsgroup'
makeDbAliases
    ''Kidsgroup'
    [ [t|KidsgroupId|]
    , [t|Name|]
    ]
makeAdaptorAndInstance' ''Kidsgroup'
makeDbTable "kidsgroups" ''Kidsgroup
