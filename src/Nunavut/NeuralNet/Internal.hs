{-# LANGUAGE TemplateHaskell #-}
module Nunavut.NeuralNet.Internal where

import Prelude hiding (reverse)

import Control.Lens (makeLenses)
import Data.Foldable (foldrM)
import Data.List.NonEmpty (NonEmpty(..), reverse)

import Nunavut.Layer
import Nunavut.Propogation
import Nunavut.Util

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
data FFNet = FFNet {
             _layers :: NonEmpty Layer
             }

makeLenses ''FFNet

{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance Show FFNet where
  show (FFNet ls) = show ls
instance SizedOperator FFNet where
  inSize = layers . _last . inSize
  outSize = layers . _head . outSize
instance Propogate FFNet where
  unsafePropogate (FFNet ls) sig = foldrM unsafePropogate sig $ reverse ls
  propogate (FFNet ls) sig = foldrM propogate sig $ reverse ls

  unsafeBackprop (FFNet ls) err = foldrM unsafeBackprop err ls
  backprop (FFNet ls) err = foldrM backprop err ls
