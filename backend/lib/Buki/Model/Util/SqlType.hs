module Buki.Model.Util.SqlType
  ( SqlType
  ) where

import Data.Text (Text)
import Data.Time (LocalTime, UTCTime)
import Data.Time.Calendar (Day)
import Data.UUID (UUID)

import qualified Opaleye as O

type family SqlType a

  -- Ids and related stuff:
type instance SqlType UUID = O.Field O.SqlUuid
type instance SqlType (Maybe UUID) = O.MaybeFields (O.Field O.SqlUuid)

-- Text:
type instance SqlType Text = O.Field O.SqlText
type instance SqlType (Maybe Text) = O.MaybeFields (O.Field O.SqlText)

-- Date and time:
type instance SqlType Day = O.Field O.SqlDate
type instance SqlType (Maybe Day) = O.MaybeFields (O.Field O.SqlDate)
type instance SqlType LocalTime = O.Field O.SqlTimestamp
type instance SqlType (Maybe LocalTime) = O.MaybeFields (O.Field O.SqlTimestamp)
type instance SqlType UTCTime = O.Field O.SqlTimestamptz
type instance SqlType (Maybe UTCTime) = O.MaybeFields (O.Field O.SqlTimestamptz)

-- Primitive types:
type instance SqlType Int = O.Field O.SqlInt4
type instance SqlType Bool = O.Field O.SqlBool
