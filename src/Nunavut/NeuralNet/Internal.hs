{-# LANGUAGE TemplateHaskell #-}
module Nunavut.NeuralNet.Internal where

import Prelude hiding (reverse)

import Control.Lens (makeLenses)
import Data.List.NonEmpty (NonEmpty)

import Nunavut.Layer
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
