{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Nunavut.Util.TestUtils where

import Data.Monoid (Monoid, mappend, mempty)
import Numeric.LinearAlgebra (dim, buildVector, buildMatrix, (@>))
import Test.Hspec
import Test.QuickCheck hiding ((===))

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

isTotal4 :: (
  Arbitrary a, Show a,
  Arbitrary b, Show b,
  Arbitrary c, Show c,
  Arbitrary d, Show d)
  => (a -> b -> c -> d -> e) -> Spec
isTotal4 f = it "is a total function" $ property $
                    \w x y z -> f w x y z `seq` True

isTotal5 :: (
  Arbitrary a, Show a,
  Arbitrary b, Show b,
  Arbitrary c, Show c,
  Arbitrary d, Show d,
  Arbitrary e, Show e)
  => (a -> b -> c -> d -> e -> f) -> Spec
isTotal5 f = it "is a total function" $ property $
                    \v w x y z -> f v w x y z `seq` True

{--------------------------------------------------------------------------
-                       Typeclass Law Verification                       -
--------------------------------------------------------------------------}
validMonoid :: forall a. (Monoid a, Show a, Eq a, Arbitrary a) =>
          a -> Spec
validMonoid _ = do
  it "obeys the left identity Monoid law" $ property $
    \(m :: a) -> mempty `mappend` m == m

  it "obeys the right identity Monoid law" $ property $
    \(m :: a) -> m `mappend` mempty == m

  it "obeys the associativity Monoid law" $ property $
    \((l,m,n) :: (a,a,a)) -> 
      l `mappend` (m `mappend` n) == (l `mappend` m) `mappend` n

{--------------------------------------------------------------------------
-                        Derivative Approxmation                         -
--------------------------------------------------------------------------}
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
