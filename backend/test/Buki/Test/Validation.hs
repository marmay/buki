module Buki.Test.Validation where

import Buki.Validation
import Buki.Types

import Test.Hspec
import qualified Data.Validation as V
import Data.Text (Text)

validationTypesTestTree :: Spec
validationTypesTestTree = do
  describe "Buki.Validation.Types" $ do
    nameTests

-- >>> validate @Text @Name "foo"
-- Success (Name {unName = "foo"})

-- >>> validate @Text @Name " bar "
-- Failure [ValidationError {validationErrorPath = [], validationErrorMessage = "Name must not start with whitespaces!"},ValidationError {validationErrorPath = [], validationErrorMessage = "Name must not end with whitespaces!"}]

nameTests :: Spec
nameTests = do
  describe "NameTests" $ do
    it "Empty fails" $ do
        validate @Text @Name ""
          `shouldBe` V.Failure [ValidationError [] "Name must be non-empty!"]
    it "Leading whitespace fails" $ do
        validate @Text @Name "  foo"
          `shouldBe` V.Failure [ValidationError [] "Name must not start with whitespaces!"]
    it "Training whitespace fails" $ do
        validate @Text @Name "foo  "
          `shouldBe` V.Failure [ValidationError [] "Name must not end with whitespaces!"]
    it "Non-printable characters fail" $ do
        validate @Text @Name "foo\tbar"
          `shouldBe` V.Failure [ValidationError [] "Name must not contain non-printable characters!"]
    it "Valid name succeeds" $ do
        validate @Text @Name "foo bar"
          `shouldBe` V.Success (Name "foo bar")
