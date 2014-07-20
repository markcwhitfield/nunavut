module Nunavut.Layer(
  Layer(..),
  weights,
  activator,
  filterL,
  ) where

import Nunavut.Layer.Internal

{--------------------------------------------------------------------------
-                              Propogation                               -
--------------------------------------------------------------------------}
{-
backpropL :: Layer -> Signal -> ErrorSignal -> Either Error (ErrorSignal, Update)
backpropL l a e = dWeights <$> dActivator <$> dFilter <$> (checkDims' e =<< checkDims l a)
  where dWeights = ((trans $ l ^. weights) <>) &&& outer a
        dActivator = elementwise (l ^. activator . activatorDeriv)
        dFilter e' = (l ^. filterL . filterDeriv $ e') <> e'
-}
