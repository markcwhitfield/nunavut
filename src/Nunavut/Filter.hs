{-# LANGUAGE TemplateHaskell #-}
module Nunavut.Filter (
  runFilter,
  runFDeriv,
  softmaxFilter,
  Filter
  ) where

import Control.Lens (makeLenses)
import Numeric.LinearAlgebra (diag, outer)

import Nunavut.Newtypes

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
data Filter = Filter {
              _runFilter :: Activation -> Activation,
              _runFDeriv :: Activation -> Jacobian
              }
makeLenses ''Filter

{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
softmaxFilter :: Filter
softmaxFilter = Filter softmax softmaxDeriv

softmax :: Activation -> Activation
softmax v = elementwise (/ l1Norm v) exponentiated
  where exponentiated = elementwise (exp . (\e -> e - lInf)) v
        lInf = infNorm v

softmaxDeriv :: Activation -> Jacobian
softmaxDeriv v = mkJacob $ diag s - (s `outer` s)
  where s = unActiv . softmax $ v
