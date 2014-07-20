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
-                              Propogation                               -
--------------------------------------------------------------------------}
predict :: FFNet -> Input -> Either Error Signal
predict n = fmap head . propogate n

backpropN :: FFNet -> Signals -> ErrorSignal -> Either Error [Update]
backpropN n as e = fmap snd . foldrM foldBackProp (e, []) $ activsAndLayers 
  where foldBackProp (a,l) (err,us) = second (: us) <$> backpropL l a err
        activsAndLayers = zip as . reverse $ n ^. layers
        
{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
addLayer :: Layer -> FFNet -> Either Error FFNet
addLayer = ifDimsMatch doAdd
  where doAdd l = over layers (l <|)
