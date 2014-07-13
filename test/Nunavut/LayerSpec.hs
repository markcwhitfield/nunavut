module Nunavut.LayerSpec where

import Data.Either (isLeft, isRight)
import Test.Hspec
import Test.QuickCheck

import Nunavut.Layer
import Nunavut.Newtypes
import Nunavut.Util.Arbitrary

spec :: Spec
spec =
  describe "propL" $ do
    it "is a total function" $ property $
      \l a -> propL l a `seq` True
    it "always succeeds on dimension match" $ property $
      \(Matching l a) ->  isRight . fmap unActiv $ propL l a
    it "always fails on dimension mismatch" $ property $
      \l a -> not (dimsMatch a l) ==> isLeft $ propL l a 
