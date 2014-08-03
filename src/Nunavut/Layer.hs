{-# LANGUAGE FlexibleContexts #-}
module Nunavut.Layer(
  propL,
  unsafePropL,
  backpropL,
  unsafeBackpropL,
  Layer(..),
  weights,
  activator,
  ) where

import Control.Lens (views)
import Control.Monad.Identity (Identity)
import Control.Monad.RWS (MonadWriter, MonadState)

import Nunavut.Layer.Internal

import Nunavut.Activator
import Nunavut.Layer.Weights
import Nunavut.Propogation
import Nunavut.Util

{--------------------------------------------------------------------------
-                              Propogation                               -
--------------------------------------------------------------------------}
unsafePropL :: Layer -> Signal -> PropResult Identity
unsafePropL l = across l unsafePropW

propL :: Layer -> Signal -> PropResult (Either Error)
propL l = across l propW

unsafeBackpropL :: Layer -> ErrorSignal -> BackpropResult Identity
unsafeBackpropL l = acrossRev l unsafeBackpropW

backpropL :: Layer -> ErrorSignal -> BackpropResult (Either Error)
backpropL l = acrossRev l backpropW

{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
across ::
  (Monad m, MonadWriter PropData m)
  => Layer
  -> (Weights -> Signal -> m Signal)
  -> Signal
  -> m Signal
across l f a = runActivator =<< runWeights a
  where runActivator = views activator propA l
        runWeights = views weights f l

acrossRev ::
  (Monad m, MonadWriter Updates m, MonadState (a, PropData) m)
  => Layer
  -> (Weights -> ErrorSignal -> m ErrorSignal)
  -> ErrorSignal
  -> m ErrorSignal
acrossRev l f a = runActivator a >>= runWeights
  where runActivator = views activator backpropA l
        runWeights = views weights f l
