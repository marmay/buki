module Buki.Test.Backend.Dummy.Common where

import Buki.Backend.Auth
import qualified Buki.Model.Types as M

import Data.UUID (nil)

nil' :: M.Id a
nil' = M.Id nil

-- | Fake authorization used for testing parts of the user backend that
-- require authorization.
admin :: FakeAuthorization '[ 'UserManagement ]
admin = fakeAuthorization
