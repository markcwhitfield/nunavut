{-# LANGUAGE TemplateHaskell #-}
module Nunavut.Filter (
  filterFunc,
  filterDeriv,
  softmax,
  noFilter,
  Filter
  ) where

import Control.Lens (makeLenses, (^.))
import Numeric.LinearAlgebra (diag, outer, ident, dim)

import Nunavut.Newtypes

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
data FilterType = None | Softmax
  deriving (Show, Eq)
data Filter = Filter {
              _filterType :: FilterType,
              _filterFunc :: Activation -> Activation,
              _filterDeriv :: Activation -> Jacobian
              }
makeLenses ''Filter

{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance Show Filter where
  show = show . (^. filterType)


{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
noFilter :: Filter
noFilter = Filter None id (mkJacob . ident . dim . unActiv)

softmax :: Filter
softmax = Filter Softmax softmaxFunc softmaxDeriv

softmaxFunc :: Activation -> Activation
softmaxFunc v = elementwise (/ l1Norm v) exponentiated
  where exponentiated = elementwise (exp . (\e -> e - lInf)) v
        lInf = infNorm v

softmaxDeriv :: Activation -> Jacobian
softmaxDeriv v = mkJacob $ diag s - (s `outer` s)
  where s = unActiv . softmaxFunc $ v
