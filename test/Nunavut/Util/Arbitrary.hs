{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Nunavut.Util.Arbitrary where

import Control.Monad (liftM, liftM3)
import Numeric.LinearAlgebra
import Test.QuickCheck hiding ((><))

import Nunavut.Activator
import Nunavut.Filter
import Nunavut.Layer
import Nunavut.Newtypes
import Nunavut.Util

data (SizedOperator a, SizedOperator b) => MatchingDims a b = Matching a b
  deriving (Show, Eq)

newtype (HasMtx a) => SmallMtx a = SmallMtx a
  deriving (Show, Eq)
newtype (HasVec a) => SmallVec a = SmallVec a
  deriving (Show, Eq)

instance (HasMtx a, Arbitrary a) => Arbitrary (SmallMtx a) where
  arbitrary = do
    rs <- choose (1,10)
    cs <- choose (1,10)
    vs <- vector (rs * cs)
    return . SmallMtx . fromMtx $ (rs >< cs) vs

instance (HasVec a, Arbitrary a) => Arbitrary (SmallVec a) where
  arbitrary = do
    rs <- choose (1,10)
    vs <- vector rs
    return . SmallVec . fromVec $ fromList vs

sizedWeights :: Int -> Int -> Gen Weights
sizedWeights rs cs = liftM (mkWeights . (rs >< cs)) $ vector (rs * cs)

sizedLayer :: Int -> Int -> Gen Layer
sizedLayer rs cs = liftM3 Layer (sizedWeights rs cs) arbitrary arbitrary

sizedActivation :: Int -> Gen Activation
sizedActivation rs = liftM (mkActiv . fromList) $ vector rs

instance Arbitrary (MatchingDims Weights Activation) where
  arbitrary = do
    (Positive cs) <- arbitrary
    (Positive rs) <- arbitrary
    wghts <- sizedWeights rs cs
    activation <- sizedActivation cs
    return . Matching wghts $ activation

instance Arbitrary (MatchingDims Layer Activation) where
  arbitrary = do
    (Positive cs) <- arbitrary
    (Positive rs) <- arbitrary
    layer <- sizedLayer rs cs
    activation <- sizedActivation cs
    return . Matching layer $ activation

instance Arbitrary Weights where
  arbitrary = do
    (Positive rs) <- arbitrary
    (Positive cs) <- arbitrary
    vs <- vector (rs * cs)
    return . mkWeights $ (rs >< cs) vs

instance Arbitrary Activation where
  arbitrary = do
    (Positive rs) <- arbitrary
    vs <- vector rs
    return . mkActiv $ fromList vs

instance CoArbitrary Activation where
  coarbitrary = variant . dim . unActiv

instance Arbitrary Filter where
  arbitrary = elements [softmax, noFilter]

instance Arbitrary Activator where
  arbitrary = elements [logistic, relu, linear, tanhActivator]

instance Arbitrary Layer where
  arbitrary = liftM3 Layer arbitrary arbitrary arbitrary
