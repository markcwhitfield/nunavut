module Nunavut.Filter (
  filterFunc,
  filterDeriv,
  softmax,
  noFilter,
  Filter
  ) where

import Numeric.LinearAlgebra (diag, outer, ident, dim, maxElement)

import Nunavut.Filter.Internal
import Nunavut.Newtypes
import Nunavut.Propogation

{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
noFilter :: Filter
noFilter = Filter None id (fromMtx . ident . dim . toVec)

softmax :: Filter
softmax = Filter Softmax softmaxFunc softmaxDeriv

softmaxFunc :: Signal -> Signal
softmaxFunc v = elementwise (/ l1Norm exponentiated) exponentiated
  where exponentiated = elementwise (exp . (\e -> e - maxV)) v
        maxV = maxElement . unSig $ v

softmaxDeriv :: Signal -> Jacobian
softmaxDeriv v = fromMtx $ diag s - (s `outer` s)
  where s = unSig . softmaxFunc $ v'
        v' = mkSig . toVec $ v
