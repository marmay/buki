module Buki.Types.Isbn
  ( Isbn(..)
  ) where

import Data.Either.Extra (eitherToMaybe)
import Data.ISBN (ISBN, validateISBN, renderISBN)
import Data.Text (Text)

import Data.Profunctor.Product.Default qualified as O
import qualified Opaleye as O

import Buki.Validation
import Buki.Model.SqlType (SqlType)

newtype Isbn = Isbn { unIsbn :: ISBN }
  deriving (Eq, Show)

type instance SqlType Isbn = O.Field O.SqlText
type instance SqlType (Maybe Isbn) = O.MaybeFields (O.Field O.SqlText)

instance Validate Text Isbn where
  validate =
    maybeToValidate
      (ValidationError { validationErrorPath = []
                       , validationErrorMessage = "Could not parse ISBN code!"
                       })
      (fmap Isbn . eitherToMaybe . validateISBN)
  unvalidate (Isbn isbn) = renderISBN isbn

instance O.DefaultFromField O.SqlText Isbn where
  defaultFromField = forceValidate @Text @Isbn <$> O.defaultFromField

instance O.Default O.ToFields Isbn (O.Field O.SqlText) where
  def = O.toToFields $ O.toFields . unvalidate @Text
