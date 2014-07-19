module Nunavut.Util.TestUtils where

import Numeric.LinearAlgebra (dim, buildVector, buildMatrix, (@>))
import Test.Hspec
import Test.QuickCheck

import Nunavut.Newtypes

isTotal :: (Arbitrary a, Show a) => (a -> b) -> Spec
isTotal f = it "is a total function" $ property $
                    \z -> f z `seq` True

isTotal2 :: (Arbitrary a, Show a, Arbitrary b, Show b) => (a -> b -> c) -> Spec
isTotal2 f = it "is a total function" $ property $
                    \y z -> f y z `seq` True

isTotal3 :: (Arbitrary a, Show a, Arbitrary b, Show b, Arbitrary c, Show c) => (a -> b -> c -> d) -> Spec
isTotal3 f = it "is a total function" $ property $
                    \x y z -> f x y z `seq` True

eps :: Double
eps = 10 ** (-5)

diff :: (Double -> Double) -> Double -> Double
diff f x = (f (x + eps) - f x) / eps

diffJacob :: HasVec a => (a -> a) -> a -> Jacobian
diffJacob f a = mkJacob $ buildMatrix (dim v) (dim v) fwdDiffs
  where v = toVec a
        f' i = (@> i) . toVec . f . fromVec
        fwdDiffs (i,j) = (f' i (v + epsVec (dim v) j) - f' i v) / eps
        epsVec d idx = buildVector d (\i -> if i == idx then eps else 0)
