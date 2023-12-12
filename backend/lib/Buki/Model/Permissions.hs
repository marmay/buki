module Buki.Model.Permissions
  ( SqlPermissions
  , Permissions(..)
  ) where

import Opaleye ( DefaultFromField(..), FromField, ToFields, Field )
import Opaleye.Internal.Inferrable ( Inferrable(Inferrable) )
import Opaleye.Experimental.Enum ( EnumMapper(..), enumMapper )
import Data.Profunctor.Product.Default (Default(..))
import Buki.Model.Util.SqlType (SqlType)

data SqlPermissions
data Permissions
  = Nobody
  | Registered
  | Admin
  deriving (Eq, Ord, Enum, Show)

toSqlPermissionsString :: Permissions -> String
toSqlPermissionsString p = case p of
  Nobody        -> "nobody"
  Registered    -> "registered"
  Admin         -> "admin"
fromSqlPermissionsString :: String -> Maybe Permissions
fromSqlPermissionsString s = case s of
  "nobody"      -> Just Nobody
  "registered"  -> Just Registered
  "admin"       -> Just Admin
  _             -> Nothing
sqlPermissionsMapper :: EnumMapper SqlPermissions Permissions
sqlPermissionsMapper = enumMapper "permissions"
                                   fromSqlPermissionsString
                                   toSqlPermissionsString

type instance SqlType Permissions = Field SqlPermissions

instance DefaultFromField SqlPermissions Permissions where
  defaultFromField = enumFromField sqlPermissionsMapper
instance permissions ~ Permissions
  => Default (Inferrable FromField) SqlPermissions permissions where
    def = Inferrable def
instance Default ToFields Permissions (Field SqlPermissions) where
  def = enumToFields sqlPermissionsMapper

