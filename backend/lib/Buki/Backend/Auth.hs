module Buki.Backend.Auth (
  AuthorizationPermission (..),
  Authorization (..),
  FakeAuthorization(..),
  HasPermissions(..),
  fakeAuthorization
) where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Set as S

import Buki.Types.Name (Name (..))
import Buki.Types.EmailAddress (EmailAddress (..))
import Buki.Model.Types (UserId)

-- | Users are having roles. Each role grants a set of permissions.
-- Some functions require users to have particular permissions.
data AuthorizationPermission
  = UserManagement
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Constraint that checks for a prove that the user has a particular set of permissions.
-- This type class is properly implemented by the Session backend, which is the only
-- issuer of authorization proves in production, c.f. `Authorization`.
-- The type class is also implemented by for testing purposes in order to fake any required
-- set of permissions without actually having to have a valid user.
class auth `HasPermissions` (ps :: [AuthorizationPermission]) where
  -- | Gets the user id (required to perform self-management operations).
  authUserId :: auth -> Proxy ps -> Maybe UserId
  
  -- | Generate an identifier for the authorization for internal logging purposes.
  authLogIdentifier :: auth -> Proxy ps -> Text

-- | An authorization is a proof that a user has a given set of roles. Backend
-- functions require that proof to be passed as an argument. By this, we basically
-- encode the authorization into the type system and push the handling of authorization
-- errors out of the business logic towards the API layer or the frontend.
data Authorization (r :: [AuthorizationPermission]) where
  Authorization :: UserId -> Name -> EmailAddress -> S.Set AuthorizationPermission -> Authorization r

class (a :: AuthorizationPermission) `IsIn` (as :: [AuthorizationPermission])
instance {-# OVERLAPS #-} a `IsIn` (a ': as)
instance {-# OVERLAPPABLE #-} forall (a :: AuthorizationPermission) (b :: AuthorizationPermission) (as :: [AuthorizationPermission]). (a `IsIn` as) => a `IsIn` (b ': as)

class (as :: [AuthorizationPermission]) `Subset` (bs :: [AuthorizationPermission])
instance '[] `Subset` bs
instance (a `IsIn` bs, as `Subset` bs) => (a ': as) `Subset` bs

instance forall (ps :: [AuthorizationPermission]) (ps' :: [AuthorizationPermission]). ps' `Subset` ps => Authorization ps `HasPermissions` ps' where
  authUserId (Authorization userId _ _ _) _ = Just userId
  authLogIdentifier (Authorization _ name emailAddress _) _ = unName name <> " (" <> unEmailAddress emailAddress <> ")"

data FakeAuthorization (ps :: [AuthorizationPermission]) where
  FakeAuthorization :: FakeAuthorization ps
instance forall (ps :: [AuthorizationPermission]) (ps' :: [AuthorizationPermission]). ps' `Subset` ps => FakeAuthorization ps `HasPermissions` ps' where
  authUserId FakeAuthorization _ = Nothing
  authLogIdentifier _ _ = "fake authorization"

-- | Creates an authorization out of thin air. Provided for testing purposes only.
fakeAuthorization ::
  forall (permissions :: [AuthorizationPermission]).
  FakeAuthorization permissions
fakeAuthorization = FakeAuthorization
