module Buki.Types.Password (Password(..)) where

import Buki.Validation

import Data.Char qualified as C
import Data.Text (Text)
import Data.Text qualified as T

import Data.Profunctor.Product.Default qualified as O
import Opaleye qualified as O

newtype Password = Password {unPassword :: Text}
  deriving (Eq, Show)

instance Validate Text Password where
  validate =
    mkValidated
      Password
      [
        ( "Password must not contain non-printable characters!"
        , all (\c -> c == ' ' || C.isPrint c) . T.unpack
        )
      ,
        ( "Password must be at least 8 characters long!"
        , (>= 8) . T.length
        )
      ]
  unvalidate (Password pwd) = pwd

instance O.DefaultFromField O.SqlText Password where
  defaultFromField = forceValidate @Text @Password <$> O.defaultFromField
  
instance O.Default O.ToFields Password (O.Field O.SqlText) where
  def = O.toToFields $ O.toFields . unvalidate @Text
