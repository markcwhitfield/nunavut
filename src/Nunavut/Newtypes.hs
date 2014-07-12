{-# LANGUAGE MultiParamTypeClasses #-}
module Nunavut.Newtypes (
  Activation,
  ErrorSignal,
  Weights,
  Error,
  mkActiv,
  mkErrSig,
  mkWeights,
  mkError,
  unActiv,
  unErrSig,
  unWeights,
  unError,
  DimMatchable (..)
  )where

import Control.Lens (to)
import Data.Text.Lazy (Text)
import Numeric.LinearAlgebra (Matrix, Vector, dim, cols, rows)

import Nunavut.Util.Dimensions

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
newtype Activation = Activ { unActiv :: Vector Double }
newtype ErrorSignal = ErrSig { unErrSig :: Vector Double }
newtype Weights = Weights { unWeights :: Matrix Double }

newtype Error = Error { unError :: Text }


{--------------------------------------------------------------------------
-                              Contructors                               -
--------------------------------------------------------------------------}
mkActiv :: Vector Double -> Activation
mkActiv = Activ
mkErrSig :: Vector Double -> ErrorSignal
mkErrSig = ErrSig
mkWeights :: Matrix Double -> Weights
mkWeights = Weights

mkError :: Text -> Error
mkError = Error


{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance HasOutput Weights where
  outSize = to $ cols . unWeights
instance HasInput Weights where
  inSize = to $ rows . unWeights

instance DimMatchable Weights Activation where
  dimsMatch (Weights m) (Activ v)
    | dim v == cols m = True
    | otherwise       = False
