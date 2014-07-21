{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Nunavut.Newtypes (
  Input,
  Label,
  Jacobian,
  mkInput,
  mkJacob,
  mkLabel,
  unInput,
  unLabel,
  unJacob,
  l1Norm,
  l2Norm,
  infNorm,
  frobNorm,
  elementwise,
  HasVec(..),
  HasMtx(..),
  (<>),
  trans,
  ) where

import Control.Lens (to)
import Numeric.LinearAlgebra (
  Matrix, Vector, dim, cols, rows, NormType(..),
  mapVector, pnorm)
import qualified Numeric.LinearAlgebra as LA

import Nunavut.Util.Dimensions

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
newtype Input = Input { unInput :: Vector Double }
  deriving (Show, Eq, Ord, Num)
newtype Label = Label { unLabel :: Vector Double }
  deriving (Eq, Show, Ord, Num)

newtype Jacobian = Jacob { unJacob :: Matrix Double }
  deriving (Show, Eq)
data Norm = L1 | L2 | InfNorm | Frob

{--------------------------------------------------------------------------
-                                Classes                                 -
--------------------------------------------------------------------------}
class HasVec a where
  toVec :: a -> Vector Double
  fromVec :: Vector Double -> a
  pNorm :: Norm -> a -> Double
  pNorm norm a = pnorm (toNormType norm) (toVec a)
class HasMtx a where
  toMtx :: a -> Matrix Double
  fromMtx :: Matrix Double -> a
class Mul a b c | a b -> c where
  (<>) :: a -> b -> c


{--------------------------------------------------------------------------
-                              Contructors                               -
--------------------------------------------------------------------------}
mkInput :: Vector Double -> Input
mkInput = Input
mkLabel :: Vector Double -> Label
mkLabel = Label

mkJacob :: Matrix Double -> Jacobian
mkJacob = Jacob

{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance SizedOperator Jacobian where
  outSize = to $ rows . unJacob
  inSize = to $ cols . unJacob

instance SizedOperator Label where
  outSize = to $ dim . unLabel
  inSize = outSize

instance SizedOperator Input where
  outSize = to $ dim . unInput
  inSize = outSize

instance HasVec Input where
  toVec = unInput
  fromVec = mkInput
instance HasVec Label where
  toVec = unLabel
  fromVec = mkLabel

instance HasMtx Jacobian where
  toMtx = unJacob
  fromMtx = mkJacob

instance (HasMtx a, HasVec b) => Mul a b b where
  a <> b = fromVec $ toMtx a LA.<> toVec b
instance (HasMtx a) => Mul a a a where
  a <> b = fromMtx $ toMtx a LA.<> toMtx b
instance (HasVec a) => Mul a a Double where
  a <> b = toVec a LA.<.> toVec b

{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
wrapM :: (HasMtx a) => (Matrix Double -> Matrix Double) -> a -> a
wrapM f = fromMtx . f . toMtx

trans :: (HasMtx a) => a -> a
trans = wrapM LA.trans

l1Norm :: (HasVec a) => a -> Double
l1Norm = pNorm L1

l2Norm :: (HasVec a) => a -> Double
l2Norm = pNorm L2

infNorm :: (HasVec a) => a -> Double
infNorm = pNorm InfNorm

frobNorm :: (HasVec a) => a -> Double
frobNorm = pNorm Frob

elementwise :: (HasVec a) => (Double -> Double) -> a -> a
elementwise f = fromVec . mapVector f . toVec


toNormType :: Norm -> NormType
toNormType InfNorm = Infinity
toNormType L1 = PNorm1
toNormType L2 = PNorm2
toNormType Frob = Frobenius
