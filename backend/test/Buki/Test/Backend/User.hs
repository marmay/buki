{-# LANGUAGE OverloadedStrings #-}

module Buki.Test.Backend.User where

import Buki.Backend.Kidsgroup
import Buki.Backend.User
import Buki.Eff.Db (Db, dbMkUuid)
import Buki.Model.Types qualified as M
import Buki.TestUtil.Err
import Buki.TestUtil.Psql (runDbTest)
import Buki.Types
import Buki.Validation (forceValidate)
import Data.Text (Text)

import qualified Buki.Test.Backend.Dummy as D

import Effectful

import Test.Hspec

import Buki.Model.Types (Permissions (..))
import Database.PostgreSQL.Simple (Connection)

run :: Connection -> Eff '[Kidsgroup, User, Db, IOE] a -> IO a
run conn = runEff . runDbTest conn . runUserDb . runKidsgroupDb

-- | Tests for the User effect.
backendUserTestTree :: SpecWith Connection
backendUserTestTree = do
  describe "Buki.Backend.User: register" $ do
    it "registers a new user" $ \conn ->
      run conn $ do
        (kidsgroupId, _) <- D.makeTestUser
        users <- listUsers D.admin
        liftIO $ users `shouldBe` [D.listUser{listUserKidsgroupId = kidsgroupId}]

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
        users <- listUsers D.admin
        liftIO $ users `shouldBe` []

    it "register with same email fails" $ \conn -> do
      run conn $ do
        (kidsgroupId, _) <- D.makeTestUser
        registerUser D.testUser{registerDataName = Name "Other name", registerDataKidsgroup = kidsgroupId}
          >>= assertError EmailAddressTakenError
        users <- listUsers D.admin
        liftIO $ users `shouldBe` [D.listUser{listUserKidsgroupId = kidsgroupId}]

  describe "Buki.Backend.User: authentication" $ do
    it "Login with wrong email address fails" $ \conn -> do
      run conn $ do
        authenticateUser (EmailAddress "wrong@wrong") (Password "test")
          >>= assertError Buki.Backend.User.InvalidUserOrPasswordError
        pure ()
    it "Login with wrong password fails" $ \conn -> do
      run conn $ do
        _ <- D.makeTestUser
        authenticateUser (registerDataEmail D.testUser) (Password "wrong")
          >>= assertError Buki.Backend.User.InvalidUserOrPasswordError
        pure ()
    it "Login with correct password succeeds" $ \conn -> do
      run conn $ do
        _ <- D.makeTestUser
        _ <-
          authenticateUser (registerDataEmail D.testUser) (registerDataPassword D.testUser)
            >>= assertSuccess
        pure ()

  describe "Buki.Backend.User: changeUserPermissons" $ do
    it "Fails if user does not exist" $ \conn -> do
      run conn $ do
        uuid <- dbMkUuid
        _ <-
          changeUserPermissions D.admin (M.Id uuid) Admin
            >>= assertError Buki.Backend.User.InvalidUserIdError
        pure ()
    it "Succeeds if user exists" $ \conn -> do
      run conn $ do
        (kidsgroupId, userId) <- D.makeTestUser
        _ <-
          changeUserPermissions D.admin userId Admin
            >>= assertSuccess
        users <- listUsers D.admin
        liftIO $ users `shouldBe` [D.listUser{listUserKidsgroupId = kidsgroupId, listUserPermissions = Admin}]
