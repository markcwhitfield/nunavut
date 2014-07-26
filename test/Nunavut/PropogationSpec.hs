module Nunavut.PropogationSpec where

import Test.Hspec

import Nunavut.Propogation
import Nunavut.Util.Arbitrary ()
import Nunavut.Util.TestUtils

spec :: Spec
spec =
  describe "PropData" $
    validMonoid (undefined :: PropData)
