{-# LANGUAGE ScopedTypeVariables #-}
module Nunavut.Newtypes.Laws where

import Numeric.LinearAlgebra (pnorm)
import Test.Hspec
import Test.QuickCheck

import Nunavut.Newtypes
import Nunavut.Newtypes.Internal
import Nunavut.Util.Arbitrary ()

validHasVec :: forall a. (HasVec a, Eq a, Show a, Arbitrary a) => a -> Spec
validHasVec _ = do
  it "obeys the inverse law of HasVec" $ property $
    \(v :: a) -> (fromVec . toVec) v == v

  it "obeys the norm-invariance law of HasVec" $ property $
    \(v :: a) (n :: Norm) -> pNorm n v == pnorm (toNormType n) (toVec v)

validHasMtx :: forall a. (HasMtx a, Eq a, Show a, Arbitrary a) => a -> Spec
validHasMtx _ =
  it "obeys the inverse law of HasMtx" $ property $
    \(m :: a) -> (fromMtx . toMtx) m == m
