module Buki.Test.Backend.Dummy.Kidsgroup where

import Buki.Types
import Buki.Validation (forceValidate)

import Data.Text (Text)

-- | Name of the kidsgroup that we create in the test database.
kidsgroupName :: Name
kidsgroupName = forceValidate ("Test Kidsgroup" :: Text)

