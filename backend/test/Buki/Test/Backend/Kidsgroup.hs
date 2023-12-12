module Buki.Test.Backend.Kidsgroup where

import Buki.Backend.Auth
import Buki.Backend.Kidsgroup
import Buki.Eff.Db (Db)

import Buki.TestUtil.Err
import Buki.TestUtil.Psql (runDbTest)

import Buki.Model qualified as M
import Buki.Types

import Control.Lens ((^.))

import Effectful

import Test.Hspec

import Database.PostgreSQL.Simple (Connection)

run :: Connection -> Eff '[Db, IOE] a -> IO a
run conn = runEff . runDbTest conn

backendKidsgroupTestTree :: SpecWith Connection
backendKidsgroupTestTree = do
  let admin = fakeAuthorization @'[ 'UserManagement ]

  describe "Buki.Backend.Kidsgroup: createKidsgroup" $ do
    it "creates a group" $ \conn -> do
      run conn $ do
        res <- createKidsgroup admin (Name "foo") >>= assertSuccess
        kidsgroups <- listKidsgroups
        liftIO $ kidsgroups `shouldBe` [ListKidsgroup (res ^. M.id) (Name "foo")]
        pure ()
    it "does not create two groups with the same name." $ \conn -> do
      run conn $ do
        r1 <- createKidsgroup admin (Name "foo") >>= assertSuccess
        createKidsgroup admin (Name "foo") >>= assertError (KidsgroupAlreadyExistsError r1)

  describe "Buki.Backend.Kidsgroup: renameKidsgroup" $ do
    it "renames a group" $ \conn -> do
      run conn $ do
        r <- (^. M.id) <$> (createKidsgroup admin (Name "foo") >>= assertSuccess)
        renameKidsgroup admin (M.toUuid r) "bar" >>= assertSuccess
        kidsgroups <- listKidsgroups
        liftIO $ kidsgroups `shouldBe` [ListKidsgroup r (Name "bar")]

  describe "Buki.Backend.Kidsgroup: removeEmptyKidsgroup" $ do
    it "deletes an empty group" $ \conn -> do
      run conn $ do
        r <- (^. M.id) <$> (createKidsgroup admin (Name "foo") >>= assertSuccess)
        kidsgroups <- listKidsgroups
        liftIO $ kidsgroups `shouldBe` [ListKidsgroup r (Name "foo")]
        _ <- removeEmptyKidsgroup admin (M.toUuid r) >>= assertSuccess
        kidsgroups' <- listKidsgroups
        liftIO $ kidsgroups' `shouldBe` []
