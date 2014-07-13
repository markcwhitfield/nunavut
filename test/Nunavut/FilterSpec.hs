module Nunavut.FilterSpec where

import Control.Lens
import Debug.Trace (traceShow)
import Numeric.LinearAlgebra (sub, flatten)
import Test.Hspec
import Test.QuickCheck

import Nunavut.Filter
import Nunavut.Newtypes
import Nunavut.Util
import Nunavut.Util.Arbitrary
import Nunavut.Util.TestUtils

spec :: Spec
spec = do
  describe "filterFunc" $ do
    it "is always a total function" $ property $
      \f v -> (f ^. filterFunc) v `seq` True
    it "never changes the dimensions of its input" $ property $
      \f v -> (f ^. filterFunc) v ^. inSize == v ^. inSize

  describe "filterDeriv" $ do
    it "is always a total function" $ property $
      \f v -> (f ^. filterDeriv) v `seq` True
    it "returns a square matrix with the dimension of its input" $ property $
      \f v -> let j = (f ^. filterDeriv) v
              in j ^. outSize == v ^. outSize && j ^. inSize == v ^. outSize
    it "returns the Jacobian of filterFunc at its input" $ property $
      \f (SmallVec v) -> let func = (f ^. filterFunc)
                             j = toMtx $ (f ^. filterDeriv) v
                             numericalJ = toMtx $ diffJacob func v
                         in  traceShow j $ traceShow numericalJ $ (l2Norm . mkActiv . flatten $ j `sub` numericalJ) < eps

  describe "softmaxFunc" $
    it "always returns a vector of L1 == 1" $ property $
      \v -> 1 - (abs . l1Norm) (softmax ^. filterFunc $ v) < eps
