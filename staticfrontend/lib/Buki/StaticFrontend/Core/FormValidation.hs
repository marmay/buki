{-# LANGUAGE OverloadedStrings #-}

-- | Validate data from HTML forms.
module Buki.StaticFrontend.Core.FormValidation (
  ValidateFromForm (..),
  FormValidation (..),
  FormValidationData (..),
  formValidationErrorsOf,
  formValidationValueOf,
  formValidate,
) where

import Buki.Validation
import Data.Map qualified as M
import Data.Text (Text)
import Data.Validation (Validation (..))
import Web.FormUrlEncoded (Form, FromForm (..), parseUnique)

class ValidateFromForm a where
  validateFromForm :: Form -> FormValidation a

data FormValidationData = FormValidationData
  { formValidationDataValues :: M.Map Text Text
  , formValidationDataErrors :: M.Map Text [Text]
  }
  deriving (Show, Eq)

formValidationErrorsOf :: Text -> FormValidationData -> [Text]
formValidationErrorsOf field (FormValidationData _ errors) =
  M.findWithDefault [] field errors

formValidationValueOf :: Text -> FormValidationData -> Maybe Text
formValidationValueOf field (FormValidationData values _) =
  M.lookup field values

instance Semigroup FormValidationData where
  FormValidationData values1 errors1 <> FormValidationData values2 errors2 =
    FormValidationData (M.union values1 values2) (M.unionWith (++) errors1 errors2)
instance Monoid FormValidationData where
  mempty = FormValidationData M.empty M.empty

data FormValidation a = FormValidation
  { formValidationData :: FormValidationData
  , formValidationResult :: Maybe a
  }
  deriving (Show, Eq, Functor)

instance Applicative FormValidation where
  FormValidation d f <*> FormValidation d' a =
    FormValidation (d <> d') (f <*> a)
  pure x = FormValidation mempty (Just x)

formValidate :: forall a. (Validate Text a) => Text -> Form -> FormValidation a
formValidate field form = case parseUnique @Text field form of
  Left _ ->
    FormValidation @a
      ( FormValidationData
          M.empty
          ( M.singleton
              field
              ["Der Wert fehlt, kommt mehrfach vor oder ist ungültig formatiert!"]
          )
      )
      Nothing
  Right a -> case validate a of
    Failure errs ->
      FormValidation @a
        (FormValidationData (M.singleton field a) (M.singleton field (map validationErrorMessage errs)))
        Nothing
    Success b ->
      FormValidation @a
        (FormValidationData (M.singleton field a) M.empty)
        (Just b)

instance (ValidateFromForm a) => FromForm (FormValidation a) where
  fromForm = pure . validateFromForm
