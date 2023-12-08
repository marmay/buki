module Buki.Model.Types.User
  ( User'(..)
  , User
  , UserId
  , UserField
  , pUser
  ) where

import Buki.Model.SqlType (SqlType)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance')
import Data.Text (Text)
import Data.Time (LocalTime)
import Buki.Model.Types.Permissions ( Permissions )

import Buki.Model.TH

import Buki.Model.Types.Id
import Buki.Model.Types.Kidsgroup (Kidsgroup')

import Buki.Types

data User' t1 t2 t3 t4 t5 t6 t7 t8 t9 = User
  { user'Id :: t1
  -- ^ Primary key.
  , user'Email :: t2
  -- ^ Email address of the user; must also be unique.
  , user'Name :: t3
  -- ^ First and last name as chosen by the user.
  , user'PasswordHash :: t4
  -- ^ Hash of the user's password; created during registration.
  , user'FailedLoginAttempts :: t5
  -- ^ Number of failed login attempts since the last successful login.
  , user'LockedAt :: t6
  -- ^ Time when the user was locked out due to too many failed login attempts.
  , user'Permissions :: t7
  -- ^ Permissions of the user; relatively coarse-grained.
  , user'KidsgroupId :: t8
  -- ^ ID of the group of one of the user's children.
  , user'Kidsymbol :: t9
  -- ^ Symbol of one of the user's children in the given group.
  } deriving (Show, Eq)

type UserId = Id User'
makeDbAliases ''User' [ [t| UserId |]
                      , [t| EmailAddress |]
                      , [t| Name |]
                      , [t| Text |]
                      , [t| Int |]
                      , [t| Maybe LocalTime |]
                      , [t| Permissions |]
                      , [t| Id Kidsgroup' |]
                      , [t| Text |]
                      ]
makeAdaptorAndInstance' ''User'
