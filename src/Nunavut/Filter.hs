{-# LANGUAGE FlexibleContexts #-}
module Nunavut.Filter (
  propF,
  backpropF,
  filterFunc,
  filterDeriv,
  softmax,
  noFilter,
  Filter
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (ask, MonadReader)
import Control.Monad.Writer (tell, MonadWriter)
import Data.Monoid (mempty)
import Numeric.LinearAlgebra (diag, outer, ident, dim, maxElement)

import Nunavut.Filter.Internal
import Nunavut.Newtypes
import Nunavut.Propogation

{--------------------------------------------------------------------------
-                              Propogation                               -
--------------------------------------------------------------------------}
propF :: (Monad m, MonadWriter PropData m)
  => Filter -> Signal -> m Signal
propF f sig = do
  tell $ PData mempty mempty [sig]
  return $ (f ^. filterFunc) sig

backpropF :: (
  Monad m,
  MonadWriter Updates m,
  MonadReader PropDatum m)
  => Filter -> ErrorSignal -> m ErrorSignal
backpropF f err = do
  datum <- ask
  let jac = (f ^. filterDeriv) (datum ^. dPreFiltered)
  return $ trans jac <> err

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
