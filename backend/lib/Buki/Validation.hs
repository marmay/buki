module Buki.Validation (
  ValidationError (..),
  Validate (..),
  mkValidated,
  validateWithPath,
  assertValid,
  forceValidate,
  maybeToValidate
) where

-- \^ The validation module serves a simple purpose: It validates (or parses) a raw,
-- unvalidated type (like Text) into a strong, validated type (like Email). If the
-- validation fails for some reason, it shall return a list of reasons, why it failed
-- for that particular value.
--
-- We use Data.Validation as the applicative that makes this fly, but provide a
-- custom error type and type class that also allows for convenient validation of
-- records, as well as some helper functions.

import Data.Text (Text)
import Data.UUID (UUID, fromText, toText)
import Data.Validation (Validation (..))

import Data.Maybe (mapMaybe)

-- | A validation error contains a path to the value that failed validation and
-- a message that describes the error. For simple values, the path will be empty.
data ValidationError = ValidationError
  { validationErrorPath :: [Text]
  , validationErrorMessage :: Text
  }
  deriving (Eq, Show)

-- | The Validate type class is used to validate a raw type (u) into a validated
-- type (v). It also provides a way to unvalidate a validated type back into an
-- unvalidated type.
class Validate u v where
  -- | Validate a raw value into a validated value. If the validation fails, it
  -- returns a list of validation errors.
  validate :: u -> Validation [ValidationError] v

  -- | Unvalidate a validated value back into a raw value.
  unvalidate :: v -> u

-- | A helper function that makes it easy to implement the validate function for
-- simple types. It takes a constructor function that constructs the validated
-- type from the raw type and a list of tests that the raw type must pass in order
-- to be valid.
--
-- The tests are a list of pairs of a message and a predicate. If the predicate
-- returns True, the test passes. If it returns False, the test fails and the
-- message is added to the list of validation errors.
--
-- For all ValidationErrors that are returned, the path will be empty.
--
-- Example:
--
-- > newtype Email = Email Text deriving (Eq, Show)
-- >
-- > instance Validate Text Email where
-- >   validate = mkValidated Email
-- >     [ ("Contains an @ sign", Text.isInfixOf "@")
-- >     ]
-- >   unvalidate (Email t) = t
mkValidated :: (u -> v) -> [(Text, u -> Bool)] -> u -> Validation [ValidationError] v
mkValidated c tests u = case mapMaybe
  ( \(msg, p) ->
      if p u
        then Nothing
        else Just $ ValidationError [] msg
  )
  tests of
  [] -> Success $ c u
  errs -> Failure errs

-- | A helper function that makes it easy to implement the validate function for
-- records. It takes the name of the field that is being validated and the value
-- of that field. It then validates the value (just like the validate function),
-- but adds the field name to the path of all validation errors that are returned.
--
-- Example:
--
-- > data User' a b = User
-- >   { userEmail :: a
-- >   , userPassword :: b
-- >   } deriving (Eq, Show)
-- >
-- > type User = User' Email Password
-- > type UserRaw = User' Text Text
-- >
-- > instance Validate UserRaw User where
-- >  validate (User userEmail userPassword) =
-- >   User <$> validateWithPath "email" userEmail
-- >        <*> validateWithPath "password" userPassword
-- >  unvalidate (User email password) = User (unvalidate email) (unvalidate password)
validateWithPath :: Validate u v => Text -> u -> Validation [ValidationError] v
validateWithPath path u = case validate u of
  Failure errs -> Failure $ map (\err -> err{validationErrorPath = path : validationErrorPath err}) errs
  Success v -> Success v

-- | Asserts that a validation succeeded; otherwise, it throws an error.
assertValid :: Validation err a -> a
assertValid (Success a) = a
assertValid (Failure _) = error "assertValid: Validation failed!"

-- | Like validate, but throws an error if the validation fails, c.f. assertValid.
forceValidate :: forall source target. (Validate source target) => source -> target
forceValidate = assertValid . validate

maybeToValidate :: ValidationError -> (u -> Maybe v) -> u -> Validation [ValidationError] v
maybeToValidate e f u = case f u of
  Just v -> Success v
  Nothing -> Failure [e]

instance Validate Text UUID where
  validate = maybeToValidate
    (ValidationError { validationErrorPath = []
                     , validationErrorMessage = "Could not parse UUID!"
                     })
    fromText

  unvalidate :: UUID -> Text
  unvalidate = toText
