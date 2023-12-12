module Buki.Model.Id (
  Id (..),
  toUuid,
) where

import Data.Profunctor.Product.Default qualified as D
import Data.UUID (UUID)
import Opaleye qualified as O
import Buki.Model.Util.SqlType (SqlType)

newtype Id a = Id UUID
  deriving (Eq, Show)

toUuid :: Id a -> UUID
toUuid (Id uuid) = uuid

type instance SqlType (Id a) = O.Field O.SqlUuid
type instance SqlType (Maybe (Id a)) = O.MaybeFields (O.Field O.SqlUuid)

instance D.Default O.ToFields (Id a) (O.Field O.SqlUuid) where
  def = O.toToFields $ O.sqlUUID . toUuid

instance O.DefaultFromField O.SqlUuid (Id a) where
  defaultFromField = Id <$> O.defaultFromField
