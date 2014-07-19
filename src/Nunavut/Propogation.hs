module Nunavut.Propogation where

import Prelude hiding (reverse, zip)

import Control.Applicative ((<$>))
import Control.Arrow ((&&&), second)
import Control.Lens ((^.))
import Control.Monad (liftM)
import Data.Foldable (foldrM)
import Data.List.NonEmpty (NonEmpty(..), (<|), reverse, zip)

import Nunavut.Activator
import Nunavut.Filter
import Nunavut.Layer
import Nunavut.NeuralNet
import Nunavut.Newtypes
import Nunavut.Util

{--------------------------------------------------------------------------
-                          Forward Propogation                           -
--------------------------------------------------------------------------}
propogate :: FFNet -> Input -> Either Error Activations
propogate n i = foldrM foldProp (i':|[]) $ n ^. layers
  where foldProp l as@(a:|_) = liftM (<| as) $ propL l a
        i'                   = mkActiv . unInput $ i

propL :: Layer -> Activation -> Either Error Activation
propL l a = filter' <$> elementwise activator' <$> (weights' <>) <$> checkDims l a
  where filter'    = l ^. filterL . filterFunc
        activator' = l ^. activator . activatorFunc
        weights'   = l ^. weights

backpropogate :: FFNet -> Activations -> ErrorSignal -> Either Error [Update]
backpropogate n as e = fmap snd . foldrM foldBackProp (e, []) $ activsAndLayers 
  where foldBackProp (a,l) (err,us) = second (: us) <$> backpropL l a err
        activsAndLayers = zip as . reverse $ n ^. layers
        
backpropL :: Layer -> Activation -> ErrorSignal -> Either Error (ErrorSignal, Update)
backpropL l a e = dWeights <$> dActivator <$> dFilter <$> checkDims' e l
  where dWeights = ((trans $ l ^. weights) <>) &&& outer a
        dActivator = elementwise (l ^. activator . activatorDeriv)
        dFilter e' = (l ^. filterL . filterDeriv $ e') <> e'
