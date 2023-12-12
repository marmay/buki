module Buki.Types.Name (Name (..)) where

import Buki.Validation

import Data.Char qualified as C
import Data.Text (Text)
import Data.Text qualified as T

import Data.Profunctor.Product.Default qualified as O
import Opaleye qualified as O

import Buki.Model.Util.SqlType (SqlType)

newtype Name = Name {unName :: Text}
  deriving (Eq, Show)

type instance SqlType Name = O.Field O.SqlText

instance Validate Text Name where
  validate =
    mkValidated
      Name
      [ ("Name must be non-empty!", not . T.null)
      , ("Name must not start with whitespaces!", not . (" " `T.isPrefixOf`))
      , ("Name must not end with whitespaces!", not . (" " `T.isSuffixOf`))
      , ("Name must not contain non-printable characters!", all (\c -> c == ' ' || C.isPrint c) . T.unpack)
      ]
  unvalidate (Name name) = name

instance O.DefaultFromField O.SqlText Name where
  defaultFromField = forceValidate @Text @Name <$> O.defaultFromField

instance O.Default O.ToFields Name (O.Field O.SqlText) where
  def = O.toToFields $ O.toFields . unvalidate @Text
