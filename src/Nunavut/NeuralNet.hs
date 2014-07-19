module Nunavut.NeuralNet (
  FFNet,
  oneLayer,
  mkFFNet,
  layers,
  addLayer) where

import Nunavut.NeuralNet.Internal

import Data.List.NonEmpty (NonEmpty(..), (<|))
import Control.Lens (over)

import Nunavut.Layer
import Nunavut.Util

{--------------------------------------------------------------------------
-                              Constructors                              -
--------------------------------------------------------------------------}
oneLayer :: Layer -> FFNet
oneLayer l = FFNet (l :| [])

mkFFNet :: [Layer] -> Either Error FFNet
mkFFNet [] = Left $ mkError "Cannot instantiate empty FFNet"
mkFFNet (l:[]) = Right . oneLayer $ l
mkFFNet (l:ls) = addLayer l =<< mkFFNet ls


{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
addLayer :: Layer -> FFNet -> Either Error FFNet
addLayer = ifDimsMatch doAdd
  where doAdd l = over layers (l <|)
