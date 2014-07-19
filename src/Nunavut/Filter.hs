{-# LANGUAGE TemplateHaskell #-}
module Nunavut.Filter (
  filterFunc,
  filterDeriv,
  softmax,
  noFilter,
  Filter
  ) where

import Control.Lens (makeLenses, (^.))
import Numeric.LinearAlgebra (diag, outer, ident, dim, maxElement)

import Nunavut.Newtypes hiding (outer)

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
data FilterType = None | Softmax
  deriving (Show, Eq)
data Filter = Filter {
              _filterType :: FilterType,
              _filterFunc :: Activation -> Activation,
              _filterDeriv :: ErrorSignal -> Jacobian
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
noFilter = Filter None id (mkJacob . ident . dim . unErrSig)

softmax :: Filter
softmax = Filter Softmax softmaxFunc softmaxDeriv

softmaxFunc :: Activation -> Activation
softmaxFunc v = elementwise (/ l1Norm exponentiated) exponentiated
  where exponentiated = elementwise (exp . (\e -> e - maxV)) v
        maxV = maxElement . unActiv $ v

softmaxDeriv :: ErrorSignal -> Jacobian
softmaxDeriv v = mkJacob $ diag s - (s `outer` s)
  where s = unActiv . softmaxFunc $ v'
        v' = mkActiv . unErrSig $ v
