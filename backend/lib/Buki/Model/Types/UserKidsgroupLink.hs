module Buki.Model.Types.UserKidsgroupLink
  ( UserKidsgroupLink'(..)
  , UserKidsgroupLink
  , UserKidsgroupLinkId
  , UserKidsgroupLinkField
  , pUserKidsgroupLink
  ) where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance')
import Data.Text (Text)

import Buki.Model.SqlType (SqlType)
import Buki.Model.TH
import Buki.Model.Types.Id
import Buki.Model.Types.Kidsgroup
import Buki.Model.Types.User

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
