module Buki.Model.Types.Session (
  Session' (..),
  Session,
  SessionId,
  SessionField,
  pSession,
) where

import Buki.Model.TH
import Buki.Model.Types.Id
import Buki.Model.Types.Permissions (Permissions)
import Buki.Model.Types.User (UserId)
import Buki.Types (EmailAddress, Name)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance')
import Data.Time (UTCTime)

data Session' t1 t2 t3 t4 t5 t6 = Session
  { session'Id :: t1
  , session'UserId :: t2
  , session'Name :: t3
  , session'Email :: t4
  , session'Permissions :: t5
  , session'ExpiresAt :: t6
  }
  deriving (Eq, Show)

type SessionId = Id Session'
makeDbAliases
  ''Session'
  [ [t|SessionId|]
  , [t|UserId|]
  , [t|Name|]
  , [t|EmailAddress|]
  , [t|Permissions|]
  , [t|UTCTime|]
  ]
makeAdaptorAndInstance' ''Session'
