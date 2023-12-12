module Buki.Types.EmailAddress
  ( EmailAddress(..)) where

import Buki.Validation

import Data.Text (Text)
import Data.Text qualified as T

import Data.Profunctor.Product.Default qualified as O
import Opaleye qualified as O

import Buki.Model.Util.SqlType (SqlType)

newtype EmailAddress = EmailAddress {unEmailAddress :: Text}
  deriving (Eq, Show)

type instance SqlType EmailAddress = O.Field O.SqlText

instance Validate Text EmailAddress where
  validate =
    mkValidated
      EmailAddress
      [ ("E-Mail address must contain an @ character", ("@" `T.isInfixOf`))
      ]
  unvalidate (EmailAddress email) = email

instance O.DefaultFromField O.SqlText EmailAddress where
  defaultFromField = EmailAddress <$> O.defaultFromField

instance O.Default O.ToFields EmailAddress (O.Field O.SqlText) where
  def = O.toToFields $ O.toFields . unvalidate @Text
