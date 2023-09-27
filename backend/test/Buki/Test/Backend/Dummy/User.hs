module Buki.Test.Backend.Dummy.User where

import Buki.Backend.Kidsgroup
import Buki.Backend.User

import Buki.Eff.Db (Db)
import Buki.Validation (forceValidate)

import Buki.Model.Types qualified as M

import Buki.Test.Backend.Dummy.Common
import Buki.Test.Backend.Dummy.Kidsgroup

import Buki.TestUtil.Err (assertSuccess)

import Data.Text (Text)

import Effectful

-- | This is the user that we register in the test database.
testUser :: RegisterData
testUser =
  RegisterData
    { registerDataName = forceValidate ("Test User" :: Text)
    , registerDataPassword = forceValidate ("testtest" :: Text)
    , registerDataEmail = forceValidate ("test@test" :: Text)
    , registerDataKidsgroup = nil'
    , registerDataKidsymbol = "test"
    }

-- | This is the expected resulting ListUser entry for the test user.
listUser :: ListUser
listUser =
  ListUser
    { listUserName = registerDataName testUser
    , listUserEmail = registerDataEmail testUser
    , listUserLockedAt = Nothing
    , listUserKidsgroupId = nil'
    , listUserKidsgroupName = kidsgroupName
    , listUserKidsymbol = registerDataKidsymbol testUser
    , listUserPermissions = M.Nobody
    }

makeTestUser :: forall es. (Kidsgroup :> es, User :> es, Db :> es, IOE :> es) => Eff es (M.Id M.Kidsgroup', M.Id M.User')
makeTestUser = do
  kidsgroupId <- createKidsgroup admin kidsgroupName >>= assertSuccess
  userId <- registerUser testUser{registerDataKidsgroup = kidsgroupId} >>= assertSuccess
  pure (kidsgroupId, userId)
