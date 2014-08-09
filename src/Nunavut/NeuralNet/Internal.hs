module Nunavut.NeuralNet.Internal where

import           Prelude            hiding (reverse)

import           Control.Lens       (Lens', lens, to, views)
import           Data.List          (intercalate)
import           Data.List.NonEmpty (NonEmpty, toList)

import           Nunavut.Layer      (Layer)
import           Nunavut.Util       (SizedOperator (..), _head, _last)

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
data FFNet = FFNet {
             _layers :: NonEmpty Layer
             }

{--------------------------------------------------------------------------
-                                 Lenses                                 -
--------------------------------------------------------------------------}
layers :: Lens' FFNet (NonEmpty Layer)
layers                                 = lens _layers (\net ls -> net { _layers = ls })

{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance Show FFNet where
  show (FFNet ls)                  = intercalate "\n" . toList $ fmap show ls
instance SizedOperator FFNet where
  inSize                           = to $ views (layers . _last . inSize) pred
  outSize                          = to $ views (layers . _head . outSize) pred
