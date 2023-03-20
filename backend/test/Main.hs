module Main (main) where

-- import Test.Tasty

import Test.Hspec

import Buki.Test.Backend.Kidsgroup
import Buki.Test.Backend.User
import Buki.Test.Validation
import Buki.TestUtil.Psql (withDatabase)

main :: IO ()
main = hspec $ do
  -- Those are database tests; we typically only use a single database connection;
  -- it is the tests responsibility to set up and clean up all required tables.
  -- Typically, tests will use the runDbTest function, which sets up an empty database
  -- before the test and drops all created tables afterwards. This gives us a sufficient
  -- degree of isolation between tests while giving quite good performance.
  aroundAll withDatabase $ do
    backendKidsgroupTestTree
    backendUserTestTree
  validationTypesTestTree
