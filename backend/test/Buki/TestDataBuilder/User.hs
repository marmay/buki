{-# LANGUAGE RecordWildCards #-}

module Buki.TestDataBuilder.User where

import Buki.Backend.Kidsgroup
import Buki.Backend.User

import Buki.Types
import qualified Buki.Model.Types as M
import Effectful
import Buki.Err
import Data.Text

data TestUser = TestUser
  { testUserName :: Name
  , testUserPassword :: Password
  , testUserEmail :: EmailAddress
  , testUserKidsgroupName :: Name
  , testUserKidsymbol :: Text
  , testUserPermissions :: M.Permissions
  }
  deriving (Show, Eq)

toRegisterData :: TestUser -> M.KidsgroupId -> RegisterData
toRegisterData TestUser{..} kidsgroupId = RegisterData
  { registerDataName = testUserName
  , registerDataPassword = testUserPassword
  , registerDataEmail = testUserEmail
  , registerDataKidsgroup = kidsgroupId
  , registerDataKidsymbol = testUserKidsymbol
  }

createOrFindTestKidsgroup :: forall es. (Kidsgroup :> es) => Name -> Eff es M.KidsgroupId
createOrFindTestKidsgroup name = do
  createKidsgroup fakeAuthorization name
    `fixWithPure` \(KidsgroupAlreadyExistsError kidsgroupId) -> kidsgroupId

createTestUser :: forall es. (Kidsgroup :> es, User :> es) => TestUser -> Eff es (M.KidsgroupId, M.UserId)
createTestUser t@TestUser{..} = do
  kidsgroupId <- createOrFindTestKidsgroup testUserKidsgroupName
  userId <- registerUser (toRegisterData t kidsgroupId) >>= assertSuccess
  when (testUserPermissions /= M.Nobody) $ do
    setPermissions testUserEmail testUserPermissions >>= assertSuccess
  pure (kidsgroupId, userId)
