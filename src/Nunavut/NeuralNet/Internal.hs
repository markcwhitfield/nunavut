{-# LANGUAGE TemplateHaskell #-}
module Nunavut.NeuralNet.Internal where

import Prelude hiding (reverse)

import Control.Lens (makeLenses, to, views)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty, toList)

import Nunavut.Layer (Layer)
import Nunavut.Util (SizedOperator(..), _last, _head)

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
  show (FFNet ls) = intercalate "\n" . toList $ fmap show ls
instance SizedOperator FFNet where
  inSize = layers . _last . inSize
  outSize = to $ views (layers . _head . outSize) pred
