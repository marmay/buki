{-# LANGUAGE OverloadedStrings #-}

module Buki.Test.Backend.Session where

import Buki.Backend.Auth
import Buki.Backend.Session
import Buki.Backend.User

import qualified Buki.Test.Backend.Dummy as D
import Test.Hspec
import Database.PostgreSQL.Simple
import Effectful
import Buki.Backend.Kidsgroup
import Buki.Eff.Db
import Buki.Eff.Time
import Buki.TestUtil.Psql (runDbTest)
import Buki.TestUtil.Err
import Data.Text (Text)
import Buki.Validation

run :: Connection -> Eff '[Session, Kidsgroup, User, Db, Time, IOE] a -> IO a
run conn = runEff . runTime . runDbTest conn . runUserDb . runKidsgroupDb . runSessionDb defaultSessionSettings

backendSessionTestTree :: SpecWith Connection
backendSessionTestTree = do
  describe "Buki.Backend.Session: login" $ do
    it "Login with wrong email address fails" $ \conn -> do
      run conn $ do
        _ <- D.makeTestUser
        makeSession (forceValidate ("wrong@email" :: Text)) (registerDataPassword D.testUser)
          >>= assertError InvalidUserOrPasswordError

    it "Login with wrong password fails" $ \conn -> do
      run conn $ do
        _ <- D.makeTestUser
        makeSession (registerDataEmail D.testUser) (forceValidate ("wrongpassword" :: Text))
          >>= assertError InvalidUserOrPasswordError
          
    it "Login with correct email address and password succeeds" $ \conn -> do
      run conn $ do
        _ <- D.makeTestUser
        _ <- makeSession (registerDataEmail D.testUser) (registerDataPassword D.testUser)
          >>= assertSuccess
        pure ()

    it "Authorize for Nobody and no permissions" $ \conn -> do
      run conn $ do
        _ <- D.makeTestUser
        (sessionId, _) <- makeSession (registerDataEmail D.testUser) (registerDataPassword D.testUser)
          >>= assertSuccess
        _ <- authorize @'[] sessionId
          >>= assertSuccess
        pure ()

    it "Authorized user data correct" $ \conn -> do
      run conn $ do
        (_, userId) <- D.makeTestUser
        (sessionId, _) <- makeSession (registerDataEmail D.testUser) (registerDataPassword D.testUser)
          >>= assertSuccess
        (Authorization @'[] authorizedUser) <- authorize @'[] sessionId
          >>= assertSuccess
        liftIO $ authorizedUser `shouldBe` AuthorizedUser
          { authorizedUserId = userId
          , authorizedUserName = registerDataName D.testUser
          , authorizedUserEmail = registerDataEmail D.testUser
          , authorizedUserPermissions = mempty
          }
          
    it "Authorize for Nobody and UserManagement permissions fails" $ \conn -> do
      run conn $ do
        _ <- D.makeTestUser
        (sessionId, _) <- makeSession (registerDataEmail D.testUser) (registerDataPassword D.testUser)
          >>= assertSuccess
        authorize @'[ 'UserManagement ] sessionId
          >>= assertError LackOfPrivileges

    it "Destroying own session works" $ \conn -> do
      run conn $ do
        _ <- D.makeTestUser
        (sessionId, _) <- makeSession (registerDataEmail D.testUser) (registerDataPassword D.testUser)
          >>= assertSuccess
        auth <- authorize @'[] sessionId
          >>= assertSuccess
        v <- destroyOwnSession auth
        liftIO $ v `shouldBe` True
        authorize @'[] sessionId
          >>= assertError InvalidSessionId

    it "Destroying foreign session works" $ \conn -> do
      run conn $ do
        (_, userId) <- D.makeTestUser
        (sessionId, _) <- makeSession (registerDataEmail D.testUser) (registerDataPassword D.testUser)
          >>= assertSuccess
        v <- destroyUserSession (fakeAuthorization @'[ 'UserManagement ]) userId
        liftIO $ v `shouldBe` True
        authorize @'[] sessionId
          >>= assertError InvalidSessionId
