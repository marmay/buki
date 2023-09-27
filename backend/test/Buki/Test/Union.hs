module Buki.Test.Union where

import Test.Hspec
import Test.Hspec.QuickCheck
import Buki.Union
import Data.Maybe (isNothing)
import qualified Data.Map as M
import Data.Proxy (Proxy(..))

unionTests :: Spec
unionTests = describe "Union tests" $ do
  prop "project . inject at pos 0" $
    \x -> project @Int (inject @Int @'[Int, Bool] x) == Just x
  prop "project . inject at pos 1" $
    \x -> project @Bool (inject @Bool @'[Int, Bool] x) == Just x
  prop "project . inject failing at pos 0" $
    \x -> isNothing (project @Int (inject @Bool @'[Int, Bool] x))
  prop "project . inject failing at pos 1" $
    \x -> isNothing (project @Bool (inject @Int @'[Int, Bool] x))
  prop "project . embed . inject -- success" $
    \x -> project @Int (embed @'[Int, Bool] $ inject @Int @'[Int] x) == Just x
  prop "project . embed . inject -- success'" $
    \x -> project @Int (embed @'[Bool, Int] $ inject @Int @'[Int] x) == Just x
  prop "project . embed . inject -- fail" $
    \x -> isNothing (project @Bool (embed @'[Int, Bool] $ inject @Int @'[Int] x))
  it "embedMap id" $ do
    embedMap (Proxy @'[Int, Bool]) (Proxy @'[Int, Bool])
      `shouldBe` M.fromList [(0, 0), (1, 1)]
  it "embedMap reverse" $ do
    embedMap (Proxy @'[Int, Bool]) (Proxy @'[Bool, Int])
      `shouldBe` M.fromList [(0, 1), (1, 0)]
  it "embedMap inject1" $ do
    embedMap (Proxy @'[Int]) (Proxy @'[Bool, Int])
      `shouldBe` M.fromList [(0, 1)]
