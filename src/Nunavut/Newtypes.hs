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
  DimMatchable
  )where

import Data.Text.Lazy (Text)
import Numeric.LinearAlgebra (Matrix, Vector, dim, cols)

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
-                                Classes                                 -
--------------------------------------------------------------------------}
class DimMatchable a b where
  dimsMatch :: a -> b -> Bool

{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance DimMatchable Weights Activation where
  dimsMatch (Weights m) (Activ v)
    | dim v == cols m = True
    | otherwise       = False
