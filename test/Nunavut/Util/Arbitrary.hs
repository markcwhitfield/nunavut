{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Nunavut.Util.Arbitrary where

import Control.Lens ((^.))
import Control.Monad (liftM, liftM3)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Numeric.LinearAlgebra
import Test.QuickCheck hiding ((><))

import Nunavut.Activator
import Nunavut.Filter
import Nunavut.Layer
import Nunavut.Layer.Weights
import Nunavut.NeuralNet
import Nunavut.NeuralNet.Internal
import Nunavut.Newtypes
import Nunavut.Newtypes.Internal
import Nunavut.Propogation hiding ((><))
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

sizedFromMtx :: HasMtx a => Positive Int -> Positive Int -> Gen a
sizedFromMtx (Positive rs) (Positive cs) = liftM (fromMtx . (rs >< cs)) $ vector (rs * cs)

sizedLayer :: Positive Int -> Positive Int -> Gen Layer
sizedLayer rs cs = liftM3 Layer (sizedFromMtx rs cs) arbitrary arbitrary

sizedFromVec :: (HasVec a) => Positive Int -> Gen a
sizedFromVec (Positive rs) = liftM (fromVec . fromList) $ vector rs

sizedFFNet :: Positive Int -> Positive Int -> Gen FFNet
sizedFFNet inp (Positive 1) = do
  out <- arbitrary
  layer <- sizedLayer out inp
  return . FFNet $ (layer:|[])
sizedFFNet inp (Positive n) = do
  net@(FFNet ls) <- sizedFFNet inp (Positive $ n - 1)
  rs <- arbitrary
  layer <- sizedLayer rs (Positive $ net ^. outSize)
  return . FFNet $ layer <| ls

instance Arbitrary (MatchingDims Weights Signal) where
  arbitrary = do
    cs <- arbitrary
    rs <- arbitrary
    wghts <- sizedFromMtx rs cs
    activation <- sizedFromVec cs 
    return . Matching wghts $ activation

instance Arbitrary (MatchingDims FFNet Input) where
  arbitrary = do
    inp <- arbitrary
    numLayers <- arbitrary
    net <- sizedFFNet (Positive $ inp ^. outSize) numLayers
    return . Matching net $ inp


instance Arbitrary (MatchingDims Layer Signal) where
  arbitrary = do
    cs <- arbitrary
    rs <- arbitrary
    layer <- sizedLayer rs cs
    activation <- sizedFromVec cs
    return . Matching layer $ activation

arbMtxNewtype :: (HasMtx a) => Gen a
arbMtxNewtype = do
  rs <- arbitrary
  cs <- arbitrary
  sizedFromMtx rs cs


instance Arbitrary Weights where
  arbitrary = arbMtxNewtype

instance Arbitrary Jacobian where
  arbitrary = arbMtxNewtype

instance Arbitrary Signal where
  arbitrary = sizedFromVec =<< arbitrary

instance Arbitrary ErrorSignal where
  arbitrary = sizedFromVec =<< arbitrary

instance Arbitrary Label where
  arbitrary = sizedFromVec =<< arbitrary

instance CoArbitrary Signal where
  coarbitrary = variant . dim . unSig

instance Arbitrary Filter where
  arbitrary = elements [softmax, noFilter]

instance Arbitrary Activator where
  arbitrary = elements [logistic, relu, linear, tanhActivator]

instance Arbitrary Layer where
  arbitrary = liftM3 Layer arbitrary arbitrary arbitrary

instance Arbitrary FFNet where
  arbitrary = do
    inp <- arbitrary
    len <- arbitrary
    sizedFFNet inp len

instance Arbitrary Input where
  arbitrary = sizedFromVec =<< arbitrary

instance Arbitrary PropData where
  arbitrary = liftM3 PData arbitrary arbitrary arbitrary

instance Arbitrary Norm where
  arbitrary = elements [L1, L2, Frob, InfNorm]
