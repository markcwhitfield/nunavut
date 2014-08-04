{-# LANGUAGE FlexibleContexts #-}
module Nunavut.Layer(
  initLayer,
  propL,
  unsafePropL,
  backpropL,
  unsafeBackpropL,
  Layer(..),
  weights,
  activator,
  ) where

import Control.Applicative ((<$>))
import Control.Lens (views)
import Control.Monad.Identity (Identity)
import Control.Monad.RWS (MonadWriter, MonadState)

import Nunavut.Layer.Internal (Layer(..), activator, weights)
import Nunavut.Layer.Weights (Weights, propW, backpropW, unsafePropW, unsafeBackpropW, initWeights)


import Nunavut.Activator (Activator, propA, backpropA, unsafeBackpropA)
import Nunavut.Propogation  (PropResult, BackpropResult, PropData, Updates)
import Nunavut.Signals (Signal, ErrorSignal)
import Nunavut.Util (Error)

{--------------------------------------------------------------------------
-                              Constructor                               -
--------------------------------------------------------------------------}
initLayer :: Activator -> Int -> Int -> IO Layer 
initLayer a rs cs = Layer a <$> initWeights rs cs
{--------------------------------------------------------------------------
-                              Propogation                               -
--------------------------------------------------------------------------}
unsafePropL :: Layer -> Signal -> PropResult Identity
unsafePropL l = across l unsafePropW

propL :: Layer -> Signal -> PropResult (Either Error)
propL l = across l propW

unsafeBackpropL :: Layer -> ErrorSignal -> BackpropResult Identity
unsafeBackpropL l = acrossRev l unsafeBackpropA unsafeBackpropW

backpropL :: Layer -> ErrorSignal -> BackpropResult (Either Error)
backpropL l = acrossRev l backpropA backpropW

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
  -> (Activator -> ErrorSignal -> m ErrorSignal)
  -> (Weights -> ErrorSignal -> m ErrorSignal)
  -> ErrorSignal
  -> m ErrorSignal
acrossRev l f g a = runActivator a >>= runWeights
  where runActivator = views activator f l
        runWeights = views weights g l
