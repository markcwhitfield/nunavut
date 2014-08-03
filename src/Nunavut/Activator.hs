{-# LANGUAGE FlexibleContexts #-}
module Nunavut.Activator (
  propA,
  backpropA,
  activatorFunc,
  activatorDeriv,
  softmax,
  logistic,
  tanhActivator,
  relu,
  linear,
  Activator
  ) where

import Control.Lens ((^.), (%=), _2)
import Control.Monad.RWS (MonadState, MonadWriter, get, tell)
import Data.Monoid (mempty)
import Numeric.LinearAlgebra (diag, outer, maxElement)

import Nunavut.Activator.Internal
import Nunavut.Newtypes
import Nunavut.Propogation
import Nunavut.Signals (Signal, ErrorSignal)

{--------------------------------------------------------------------------
-                              Propogation                               -
--------------------------------------------------------------------------}
propA :: (Monad m, MonadWriter PropData m)
  => Activator -> Signal -> m Signal
propA f sig = do
  tell $ PData mempty [sig]
  return . withBias $ (f ^. activatorFunc) sig

backpropA :: (
  Monad m,
  MonadWriter Updates m,
  MonadState (a, PropData) m)
  => Activator -> ErrorSignal -> m ErrorSignal
backpropA f err = do
  (_, pData) <- get
  let jac = (f ^. activatorDeriv) (head $ pData ^. preActivated)
  _2 . preActivated %= tail
  return . withoutBias $ trans jac <> err

{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
diagActivator :: ActivatorType -> (Double -> Double) -> (Double -> Double) -> Activator
diagActivator t f d = Activator t (elementwise f) (fromMtx . diag . toVec . elementwise d)

logistic :: Activator
logistic = diagActivator Logistic logisticFunc logisticDeriv

relu :: Activator
relu = diagActivator RectifiedLinear reluFunc reluDeriv

linear :: Activator
linear = diagActivator Linear id $ const 1

tanhActivator :: Activator
tanhActivator = diagActivator Tanh tanh ((1 -) . (** 2) . tanh)

logisticFunc :: Double -> Double
logisticFunc z = 1 / (1 + exp (-z))

logisticDeriv :: Double -> Double
logisticDeriv z = s * (1 - s)
  where s = logisticFunc z
  
reluFunc :: Double -> Double
reluFunc z = max z 0

reluDeriv :: Double -> Double
reluDeriv z
  | z <= 0    = 0
  | otherwise = 1

softmax :: Activator
softmax = Activator Softmax softmaxFunc softmaxDeriv

softmaxFunc :: Signal -> Signal
softmaxFunc v = elementwise (/ l1Norm exponentiated) exponentiated
  where exponentiated = elementwise (exp . (\e -> e - maxV)) v
        maxV = maxElement . toVec $ v

softmaxDeriv :: Signal -> Jacobian
softmaxDeriv v = fromMtx $ diag s - (s `outer` s)
  where s = toVec . softmaxFunc $ v'
        v' = fromVec . toVec $ v
