module Buki.Test.Union where

import Test.Hspec
import Test.Hspec.QuickCheck
import Buki.Union
import Data.Maybe (isNothing)

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
  prop "project . embed . inject -- fail" $
    \x -> isNothing (project @Bool (embed @'[Int, Bool] $ inject @Int @'[Int] x))

