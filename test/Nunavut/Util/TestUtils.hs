module Nunavut.Util.TestUtils where

import Numeric.LinearAlgebra (dim, buildVector, buildMatrix, (@>))
import Test.Hspec
import Test.QuickCheck

import Nunavut.Newtypes

isTotal :: (Arbitrary a, Show a) => (a -> b) -> Spec
isTotal f = it "is a total function" $ property $
                    \z -> f z `seq` True

eps :: Double
eps = 0.001

diff :: (Double -> Double) -> Double -> Double
diff f x = (f (x + eps) - f x) / eps

diffJacob :: HasVec a => (a -> a) -> a -> Jacobian
diffJacob f a = mkJacob $ buildMatrix (dim v) (dim v) fwdDiffs
  where v = toVec a
        f' i = (@> i) . toVec . f . fromVec
        fwdDiffs (i,j) = (f' i (v + epsVec (dim v) j) - f' i v) / eps
        epsVec d idx = buildVector d (\i -> if i == idx then eps else 0)
