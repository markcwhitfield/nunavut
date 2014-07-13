{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Nunavut.Newtypes (
  Activation,
  ErrorSignal,
  Weights,
  Jacobian,
  mkActiv,
  mkErrSig,
  mkWeights,
  mkJacob,
  unActiv,
  unErrSig,
  unWeights,
  unJacob,
  l1Norm,
  l2Norm,
  infNorm,
  frobNorm,
  elementwise,
  HasVec(..),
  HasMtx(..),
  (<>)
  ) where

import Control.Lens (to)
import Numeric.LinearAlgebra (
  Matrix, Vector, dim, cols, rows, pnorm, NormType(..),
  mapVector)
import qualified Numeric.LinearAlgebra as LA

import Nunavut.Util.Dimensions

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
newtype Activation = Activ { unActiv :: Vector Double }
  deriving (Show, Eq, Ord)
newtype ErrorSignal = ErrSig { unErrSig :: Vector Double }
  deriving (Show, Eq, Ord)
newtype Weights = Weights { unWeights :: Matrix Double }
  deriving (Show, Eq)
newtype Jacobian = Jacob { unJacob :: Matrix Double }
  deriving (Show, Eq)

data Norm = L1 | L2 | InfNorm | Frob

{--------------------------------------------------------------------------
-                                Classes                                 -
--------------------------------------------------------------------------}
class HasNorm a where
  pNorm :: Norm -> a -> Double
class HasVec a where
  toVec :: a -> Vector Double
  fromVec :: Vector Double -> a
class HasMtx a where
  toMtx :: a -> Matrix Double
  fromMtx :: Matrix Double -> a
class Mul a b c | a b -> c where
  (<>) :: a -> b -> c


{--------------------------------------------------------------------------
-                              Contructors                               -
--------------------------------------------------------------------------}
mkActiv :: Vector Double -> Activation
mkActiv = Activ
mkErrSig :: Vector Double -> ErrorSignal
mkErrSig = ErrSig
mkWeights :: Matrix Double -> Weights
mkWeights = Weights
mkJacob :: Matrix Double -> Jacobian
mkJacob = Jacob


{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance SizedOperator Weights where
  outSize = to $ rows . unWeights
  inSize = to $ cols . unWeights

instance SizedOperator Jacobian where
  outSize = to $ rows . unJacob
  inSize = to $ cols . unJacob

instance SizedOperator Activation where
  outSize = to $ dim . unActiv
  inSize = outSize

instance HasVec Activation where
  toVec = unActiv
  fromVec = mkActiv
instance HasVec ErrorSignal where
  toVec = unErrSig
  fromVec = mkErrSig
instance HasMtx Weights where
  toMtx = unWeights
  fromMtx = mkWeights
instance HasMtx Jacobian where
  toMtx = unJacob
  fromMtx = mkJacob


instance HasNorm Activation where
  pNorm norm a = pnorm (toNormType norm) (unActiv a)
instance HasNorm ErrorSignal where
  pNorm norm e = pnorm (toNormType norm) (unErrSig e)

instance (HasMtx a, HasVec b) => Mul a b b where
  a <> b = fromVec $ toMtx a LA.<> toVec b
instance (HasMtx a) => Mul a a a where
  a <> b = fromMtx $ toMtx a LA.<> toMtx b
instance (HasVec a) => Mul a a Double where
  a <> b = toVec a LA.<.> toVec b

{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
l1Norm :: (HasNorm a) => a -> Double
l1Norm = pNorm L1

l2Norm :: (HasNorm a) => a -> Double
l2Norm = pNorm L2

infNorm :: (HasNorm a) => a -> Double
infNorm = pNorm InfNorm

frobNorm :: (HasNorm a) => a -> Double
frobNorm = pNorm Frob

elementwise :: (HasVec a) => (Double -> Double) -> a -> a
elementwise f = fromVec . mapVector f . toVec


toNormType :: Norm -> NormType
toNormType InfNorm = Infinity
toNormType L1 = PNorm1
toNormType L2 = PNorm2
toNormType Frob = Frobenius
