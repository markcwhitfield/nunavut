module Nunavut.Util.TestUtils where

import Test.Hspec
import Test.QuickCheck

isTotal :: (Arbitrary a, Show a) => (a -> b) -> Spec
isTotal f = it "is a total function" $ property $
                    \z -> f z `seq` True

eps :: Double
eps = 0.001

diff :: (Double -> Double) -> Double -> Double
diff f x = (f (x + eps) - f x) / eps
