{-# LANGUAGE TemplateHaskell #-}
module Nunavut.NeuralNet.Internal where

import Prelude hiding (foldr)

import Control.Lens (makeLenses)
import Data.Foldable (foldr, foldrM)
import Data.List.NonEmpty (NonEmpty(..))

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
  unsafePropogate (FFNet ls) sig = foldr unsafePropogate sig ls
  propogate (FFNet ls) sig = foldrM propogate sig ls
