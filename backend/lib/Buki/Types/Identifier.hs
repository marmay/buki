module Buki.Types.Identifier (Identifier (..)) where

import Buki.Validation
import Buki.Model.Util.SqlType (SqlType)

import qualified Data.Char as C
import Data.Text (Text)
import qualified Data.Text as T

import Data.Profunctor.Product.Default qualified as O
import Opaleye qualified as O

newtype Identifier = Identifier { unIdentifier :: Text }
  deriving (Eq, Show)

instance Validate Text Identifier where
  validate =
    mkValidated
      Identifier
      [ ("Identifier must not be empty!", not . T.null)
      , ("Identifier must not start with whitespaces!", not . (" " `T.isPrefixOf`))
      , ("Identifier must not end with whitespaces!", not . (" " `T.isSuffixOf`))
      , ("Identifier must not contains non-printable characters!", not . T.any (not . C.isPrint))
      ]
  unvalidate = unIdentifier

type instance SqlType Identifier = O.Field O.SqlText

instance O.DefaultFromField O.SqlText Identifier where
  defaultFromField = forceValidate @Text <$> O.defaultFromField

instance O.Default O.ToFields Identifier (O.Field O.SqlText) where
  def = O.toToFields $ O.toFields . unIdentifier
