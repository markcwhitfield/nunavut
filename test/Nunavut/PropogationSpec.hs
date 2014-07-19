module Nunavut.PropogationSpec where

import Control.Lens ((^.))
import Data.Either (isLeft, isRight)
import Test.Hspec
import Test.QuickCheck

import Nunavut.Newtypes
import Nunavut.Propogation
import Nunavut.Util
import Nunavut.Util.Arbitrary
import Nunavut.Util.TestUtils

spec :: Spec
spec = do
  describe "propL" $ do
    isTotal2 propL
    it "always succeeds on dimension match" $ property $
      \(Matching l a) ->  isRight . fmap unActiv $ propL l a
    it "always fails on dimension mismatch" $ property $
      \l a -> not (dimsMatch a l) ==> isLeft $ propL l a 
    it "always returns an Activation with dimension outSize" $ property $
      \(Matching l a) -> case propL l a of
                          Left _ -> False
                          Right b -> b ^. outSize == l ^. outSize
  
  describe "propogate" $ do
    isTotal2 propogate
    it "always succeeds on dimension match" $ property $
      \(Matching n a) ->  isRight $ propogate n a
    it "always fails on dimension mismatch" $ property $
      \n a -> not (dimsMatch a n) ==> isLeft $ propogate n a

  describe "backpropL" $
    isTotal3 backpropL
