{-# LANGUAGE FlexibleContexts #-}
module Nunavut.Activator (
  propA,
  backpropA,
  activatorFunc,
  activatorDeriv,
  logistic,
  relu,
  linear,
  tanhActivator,
  Activator
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (ask, MonadReader)
import Control.Monad.Writer (tell, MonadWriter)
import Data.Monoid (mempty)

import Nunavut.Activator.Internal
import Nunavut.Newtypes
import Nunavut.Propogation

{--------------------------------------------------------------------------
-                              Propogation                               -
--------------------------------------------------------------------------}
propA :: (Monad m, MonadWriter PropData m)
  => Activator -> Signal -> m Signal
propA a sig = do
  tell $ PData mempty [sig] mempty
  return $ elementwise (a ^. activatorFunc) sig

backpropA :: (
  Monad m,
  MonadWriter Updates m,
  MonadReader PropDatum m)
  => Activator -> ErrorSignal -> m ErrorSignal
backpropA a err = do
  datum <- ask
  let deriv = elementwise (a ^. activatorDeriv) (datum ^. dPreActivated)
  return $ err .* deriv

{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
logistic :: Activator
logistic = Activator Logistic logisticFunc logisticDeriv

relu :: Activator
relu = Activator RectifiedLinear reluFunc reluDeriv

linear :: Activator
linear = Activator Linear id $ const 1

tanhActivator :: Activator
tanhActivator = Activator Tanh tanh ((1 -) . (** 2) . tanh)

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
