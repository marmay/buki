{-# LANGUAGE UndecidableInstances #-}

module Buki.Model.UserKidsgroupLink
  ( UserKidsgroupLink'(..)
  , UserKidsgroupLink
  , UserKidsgroupLinkId
  , UserKidsgroupLinkField
  , pUserKidsgroupLink
  , userKidsgroupLinkTable
  ) where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance')
import Data.Text (Text)

import Buki.Model.Util.SqlType (SqlType)
import Buki.Model.Util.TH
import Buki.Model.Id
import Buki.Model.Kidsgroup
import Buki.Model.User

data UserKidsgroupLink' t1 t2 t3 t4 = UserKidsgroupLink
  { userKidsgroupLink'Id :: t1
  , userKidsgroupLink'UserId :: t2
  , userKidsgroupLink'KidsgroupId :: t3
  , userKidsgroupLink'KidSymbol :: t4
  }
type UserKidsgroupLinkId = Id UserKidsgroupLink'

makeDbAliases ''UserKidsgroupLink' [ [t|UserKidsgroupLinkId|]
                                   , [t|UserId|]
                                   , [t|KidsgroupId|]
                                   , [t|Text|]
                                   ]
makeAdaptorAndInstance' ''UserKidsgroupLink'
makeDbTable "user_kidsgroups" ''UserKidsgroupLink
