{-# LANGUAGE OverloadedStrings #-}

module Buki.Test.Backend.User where

import Buki.Backend.Auth
import Buki.Backend.Kidsgroup
import Buki.Backend.User
import Buki.Eff.Db (Db, dbMkUuid)
import Buki.Model.Types qualified as M
import Buki.TestUtil.Err
import Buki.TestUtil.Psql (runDbTest)
import Buki.Types
import Buki.Validation (forceValidate)
import Data.Text (Text)
import Data.UUID (nil)

import Effectful

import Test.Hspec

import Buki.Model.Types (Permissions (..))
import Data.Kind (Type)
import Database.PostgreSQL.Simple (Connection)

nil' :: M.Id a
nil' = M.Id nil

run :: Connection -> Eff '[Kidsgroup, User, Db, IOE] a -> IO a
run conn = runEff . runDbTest conn . runUserDb . runKidsgroupDb

-- | This is the user that we register in the test database.
testUser :: RegisterData
testUser =
  RegisterData
    { registerDataName = Name "Test User"
    , registerDataPassword = Password "test"
    , registerDataEmail = EmailAddress "test@test"
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
    , listUserPermissions = Nobody
    }

-- | Fake authorization used for testing parts of the user backend that
-- require authorization.
admin :: FakeAuthorization '[ 'UserManagement ]
admin = fakeAuthorization

makeTestUser :: forall es. (Kidsgroup :> es, User :> es, Db :> es, IOE :> es) => Eff es (M.Id M.Kidsgroup', M.Id M.User')
makeTestUser = do
  kidsgroupId <- createKidsgroup admin kidsgroupName >>= assertSuccess
  userId <- registerUser testUser{registerDataKidsgroup = kidsgroupId} >>= assertSuccess
  pure (kidsgroupId, userId)

-- | Name of the kidsgroup that we create in the test database.
kidsgroupName :: Name
kidsgroupName = forceValidate @Text "Test Kidsgroup"

-- | Tests for the User effect.
backendUserTestTree :: SpecWith Connection
backendUserTestTree = do
  describe "Buki.Backend.User: register" $ do
    it "registers a new user" $ \conn ->
      run conn $ do
        (kidsgroupId, _) <- makeTestUser
        users <- listUsers admin
        liftIO $ users `shouldBe` [listUser{listUserKidsgroupId = kidsgroupId}]

    it "register without kidsgroup fails" $ \conn -> do
      run conn $ do
        uuid <- dbMkUuid
        let registerData =
              RegisterData
                (forceValidate @Text "test")
                (forceValidate @Text "testtest")
                (forceValidate @Text "test@test.com")
                (M.Id uuid)
                "test"
        registerUser registerData >>= assertError Buki.Backend.User.InvalidKidsgroupIdError
        users <- listUsers admin
        liftIO $ users `shouldBe` []

    it "register with same email fails" $ \conn -> do
      run conn $ do
        (kidsgroupId, _) <- makeTestUser
        registerUser testUser{registerDataName = Name "Other name", registerDataKidsgroup = kidsgroupId}
          >>= assertError EmailAddressTakenError
        users <- listUsers admin
        liftIO $ users `shouldBe` [listUser{listUserKidsgroupId = kidsgroupId}]

  describe "Buki.Backend.User: authentication" $ do
    it "Login with wrong email address fails" $ \conn -> do
      run conn $ do
        authenticateUser (EmailAddress "wrong@wrong") (Password "test")
          >>= assertError Buki.Backend.User.InvalidUserOrPasswordError
        pure ()
    it "Login with wrong password fails" $ \conn -> do
      run conn $ do
        _ <- makeTestUser
        authenticateUser (registerDataEmail testUser) (Password "wrong")
          >>= assertError Buki.Backend.User.InvalidUserOrPasswordError
        pure ()
    it "Login with correct password succeeds" $ \conn -> do
      run conn $ do
        _ <- makeTestUser
        _ <-
          authenticateUser (registerDataEmail testUser) (registerDataPassword testUser)
            >>= assertSuccess
        pure ()

  describe "Buki.Backend.User: changeUserPermissons" $ do
    it "Fails if user does not exist" $ \conn -> do
      run conn $ do
        uuid <- dbMkUuid
        _ <-
          changeUserPermissions admin (M.Id uuid) Admin
            >>= assertError Buki.Backend.User.InvalidUserIdError
        pure ()
    it "Succeeds if user exists" $ \conn -> do
      run conn $ do
        (kidsgroupId, userId) <- makeTestUser
        _ <-
          changeUserPermissions admin userId Admin
            >>= assertSuccess
        users <- listUsers admin
        liftIO $ users `shouldBe` [listUser{listUserKidsgroupId = kidsgroupId, listUserPermissions = Admin}]
