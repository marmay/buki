{-# LANGUAGE UndecidableInstances #-}

module Buki.Model.Session (
    Session' (..),
    Session,
    SessionId,
    SessionField,
    pSession,
    sessionTable,
) where

import Buki.Model.Util.SqlType (SqlType)
import Buki.Model.Util.TH
import Buki.Model.Id
import Buki.Model.Permissions (Permissions)
import Buki.Model.User (UserId)
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
makeDbTable "sessions" ''Session
