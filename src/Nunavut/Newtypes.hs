{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Nunavut.Newtypes (
  Activation,
  ErrorSignal,
  Weights,
  Jacobian,
  Error,
  mkActiv,
  mkErrSig,
  mkWeights,
  mkError,
  mkJacob,
  unActiv,
  unErrSig,
  unWeights,
  unError,
  unJacob,
  l1Norm,
  l2Norm,
  infNorm,
  frobNorm,
  elementwise,
  DimMatchable (..)
  )where

import Control.Lens (to)
import Data.Text.Lazy (Text)
import Numeric.LinearAlgebra (
  Matrix, Vector, dim, cols, rows, pnorm, NormType(..),
  mapVector)

import Nunavut.Util.Dimensions

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
newtype Activation = Activ { unActiv :: Vector Double }
newtype ErrorSignal = ErrSig { unErrSig :: Vector Double }
newtype Weights = Weights { unWeights :: Matrix Double }
newtype Jacobian = Jacob { unJacob :: Matrix Double }

newtype Error = Error { unError :: Text }

data Norm = L1 | L2 | InfNorm | Frob

{--------------------------------------------------------------------------
-                                Classes                                 -
--------------------------------------------------------------------------}
class HasNorm a where
  pNorm :: Norm -> a -> Double
class HasVec a where
  toVec :: a -> Vector Double
  fromVec :: Vector Double -> a


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

mkError :: Text -> Error
mkError = Error


{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance HasOutput Weights where
  outSize = to $ cols . unWeights
instance HasInput Weights where
  inSize = to $ rows . unWeights

instance HasVec Activation where
  toVec = unActiv
  fromVec = mkActiv
instance HasVec ErrorSignal where
  toVec = unErrSig
  fromVec = mkErrSig

instance HasNorm Activation where
  pNorm norm a = pnorm (toNormType norm) (unActiv a)
instance HasNorm ErrorSignal where
  pNorm norm e = pnorm (toNormType norm) (unErrSig e)


instance DimMatchable Weights Activation where
  dimsMatch (Weights m) (Activ v)
    | dim v == cols m = True
    | otherwise       = False

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
