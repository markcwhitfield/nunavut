{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Nunavut.Util.Arbitrary where

import Control.Lens ((^.))
import Control.Monad (liftM, liftM2)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Monoid (mappend)
import Debug.Trace
import Numeric.LinearAlgebra
import Test.QuickCheck hiding ((><))

import Nunavut.Activator
import Nunavut.ErrorFunction
import Nunavut.Layer
import Nunavut.Layer.Weights
import Nunavut.NeuralNet
import Nunavut.NeuralNet.Internal
import Nunavut.Newtypes
import Nunavut.Newtypes.Internal
import Nunavut.Propogation hiding ((><))
import Nunavut.Signals
import Nunavut.Util

data MatchMM a b = MatchMM a b
  deriving (Show, Eq)
data MatchMV a b = MatchMV a b
  deriving (Show, Eq)
data MatchVM a b = MatchVM a b
  deriving (Show, Eq)
data MatchVV a b = MatchVV a b
  deriving (Show, Eq)

class SizedGen a where
  sizedGen :: Positive Int -> Gen a

class SizedGen2 a where
  sizedGen2 :: Positive Int -> Positive Int -> Gen a 

class SizedGen3 a where
  sizedGen3 :: Positive Int -> Positive Int -> Positive Int -> Gen a

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

newtype HasVec a => SizedV a = SizedV { unSizedV :: a }
  deriving (HasVec)
newtype HasMtx a => SizedM a = SizedM { unSizedM :: a }
  deriving (HasMtx)

instance (HasMtx a) => SizedGen2 (SizedM a) where
  sizedGen2 (Positive rs) (Positive cs) = liftM (fromMtx . (rs >< cs)) $ vector (rs * cs)

instance (HasVec a) => SizedGen (SizedV a) where
  sizedGen (Positive rs) = liftM (fromVec . fromList) $ vector rs

sizedMtx :: (HasMtx a) => Positive Int -> Positive Int -> Gen a
sizedMtx rs = fmap unSizedM . sizedGen2 rs

sizedVec :: (HasVec a) => Positive Int -> Gen a
sizedVec = fmap unSizedV . sizedGen

instance SizedGen2 Weights where
  sizedGen2 = sizedMtx

instance SizedGen Signal where
  sizedGen = sizedVec

instance SizedGen ErrorSignal where
  sizedGen = sizedVec

instance SizedGen2 Layer where
  sizedGen2 rs cs = do
    activ <- arbitrary
    (SizedM wghts) <- sizedGen2 (pred rs) cs
    return $ Layer activ wghts

instance (SizedGen2 a, SizedGen2 b) => SizedGen2 (MatchMM a b) where
  sizedGen2 rs cs = do
    inner <- arbitrary
    a <- sizedGen2 rs inner
    b <- sizedGen2 inner cs
    return $ MatchMM a b

instance (SizedGen a, SizedGen2 b) => SizedGen2 (MatchVM a b) where
  sizedGen2 rs cs = do
    a <- sizedGen rs
    b <- sizedGen2 rs cs
    return $ MatchVM a b

instance (SizedGen2 a, SizedGen b) => SizedGen2 (MatchMV a b) where
  sizedGen2 rs cs = do
    a <- sizedGen2 rs cs
    b <- sizedGen cs
    return $ MatchMV a b

instance (SizedGen a, SizedGen b) => SizedGen (MatchVV a b) where
  sizedGen rs = do
    a <- sizedGen rs
    b <- sizedGen rs
    return $ MatchVV a b

arbSGen :: (SizedGen a) => Gen a
arbSGen = sizedGen =<< arbitrary

arbSGen2 :: (SizedGen2 a) => Gen a
arbSGen2 = do
  rs <- arbitrary
  cs <- arbitrary
  sizedGen2 rs cs

instance (SizedGen2 a, SizedGen2 b) => Arbitrary (MatchMM a b) where
  arbitrary = arbSGen2

instance (SizedGen2 a, SizedGen b) => Arbitrary (MatchMV a b) where
  arbitrary = arbSGen2

instance (SizedGen a, SizedGen2 b) => Arbitrary (MatchVM a b) where
  arbitrary = arbSGen2

instance (SizedGen a, SizedGen b) => Arbitrary (MatchVV a b) where
  arbitrary = arbSGen

instance SizedGen3 FFNet where
  sizedGen3 out inp len =
    --traceShow (out,inp,len) $
    case len of
      (Positive 1) -> do
        l <- sizedGen2 (succ out) (succ inp)
        return $ FFNet (l :| [])
      _ -> do
        out' <- arbitrary
        (FFNet ls) <- sizedGen3 out' inp $ pred len
        l <- sizedGen2 (succ out) (succ out')
        return . FFNet $ l <| ls

instance SizedGen2 FFNet where
  sizedGen2 out inp = sizedGen3 out inp =<< arbitrary

matchingMtxArb :: (HasVec s, HasMtx t) => (t -> s -> b) -> Gen b
matchingMtxArb f = do
    cs <- arbitrary
    rs <- arbitrary
    (SizedM wghts) <- sizedGen2 rs cs
    (SizedV activation) <- sizedGen cs
    return . f wghts $ activation

matchingArbVec :: HasVec s => (Layer -> s -> b) -> Gen b
matchingArbVec f = do
    cs <- arbitrary
    rs <- arbitrary
    layer <- sizedGen2 rs cs
    (SizedV activation) <- sizedGen cs
    return . f layer $ activation

arbMtx :: HasMtx a => Gen a
arbMtx = do
  rs <- arbitrary
  cs <- arbitrary
  (SizedM m) <- sizedGen2 rs cs
  return m

arbVec :: HasVec a => Gen a
arbVec = do
  size <- arbitrary
  (SizedV v) <- sizedGen size
  return v

instance Arbitrary Weights where
  arbitrary = arbMtx

instance Arbitrary Jacobian where
  arbitrary = arbMtx

instance Arbitrary Signal where
  arbitrary = arbVec

instance Arbitrary ErrorSignal where
  arbitrary = arbVec

instance Arbitrary Label where
  arbitrary = arbVec

instance CoArbitrary Signal where
  coarbitrary = variant . dim . toVec

instance CoArbitrary Label where
  coarbitrary = variant . dim . toVec

instance Arbitrary Activator where
  arbitrary = elements [logistic, relu, linear, tanhActivator]

instance Arbitrary Layer where
  arbitrary = liftM2 Layer arbitrary arbitrary

instance Arbitrary FFNet where
  arbitrary = do
    inp <- arbitrary
    len <- arbitrary
    sizedGen2 inp len

instance Arbitrary Input where
  arbitrary = arbVec

instance Arbitrary PropData where
  arbitrary = do 
    len <- arbitrary
    preW <- vector len
    preA <- vector len
    return $ PData preW preA

instance Arbitrary Norm where
  arbitrary = elements [L1, L2, Frob, InfNorm]

instance Arbitrary PropConfig where
  arbitrary = do
    (Positive lr) <- arbitrary
    (Positive bs) <- arbitrary
    errF <- arbitrary
    return (PConfig lr bs errF)

instance Arbitrary ErrorFunction where
  arbitrary = elements [sumOfSquares]

data MatchLayerPData = MatchLPD Layer PropData
  deriving (Show)

instance SizedGen2 MatchLayerPData where
  sizedGen2 out inp = do
    wghts <- sizedGen2 (pred out) inp
    activ <- arbitrary
    preW <- sizedGen inp
    preA <- sizedGen (pred out)
    return $ MatchLPD (Layer activ wghts) (PData [preW] [preA])

instance Arbitrary MatchLayerPData where
  arbitrary = arbSGen2

data MatchNetPData = MatchNPD FFNet PropData
  deriving (Show)

instance SizedGen2 MatchNetPData where
  sizedGen2 out inp = sizedGen3 out inp =<< arbitrary

instance SizedGen3 MatchNetPData where
  sizedGen3 out inp len =
    case len of
      (Positive 1) -> do
        (MatchLPD l pdata) <- sizedGen2 (succ out) (succ inp)
        return $ MatchNPD (FFNet (l :| [])) pdata
      _ -> do
        out' <- arbitrary
        (MatchNPD (FFNet ls) pdata) <- sizedGen3 out' inp $ pred len
        (MatchLPD l pdatum) <- sizedGen2 (succ out) (succ out')
        return $ MatchNPD (FFNet $ l <| ls) (pdatum `mappend` pdata)
        

instance Arbitrary MatchNetPData where
  arbitrary = arbSGen2
