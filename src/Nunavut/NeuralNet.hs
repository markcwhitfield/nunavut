module Nunavut.NeuralNet (
  unsafePropogate,
  unsafeBackprop,
  propogate,
  backprop,
  FFNet,
  oneLayer,
  mkFFNet,
  layers,
  addLayer) where 
import Nunavut.NeuralNet.Internal

import Prelude hiding (reverse)

import Data.Foldable (foldrM)
import Data.List.NonEmpty (NonEmpty(..), reverse, (<|))
import Control.Lens (over)
import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Trans.Identity (IdentityT)

import Nunavut.Layer
import Nunavut.Propogation
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
unsafePropogate :: FFNet -> Signal -> PropResult IdentityT
unsafePropogate (FFNet ls) sig = foldrM unsafePropL sig $ reverse ls
propogate :: FFNet -> Signal -> PropResult (EitherT Error)
propogate (FFNet ls) sig = foldrM propL sig $ reverse ls

unsafeBackprop :: FFNet -> ErrorSignal -> BackpropResult IdentityT
unsafeBackprop (FFNet ls) err = foldrM unsafeBackpropL err ls
backprop :: FFNet -> ErrorSignal -> BackpropResult (EitherT Error)
backprop (FFNet ls) err = foldrM backpropL err ls
{-
predict :: FFNet -> Input -> Either Error Signal
predict n = fmap head . propogate n

backpropN :: FFNet -> Signals -> ErrorSignal -> Either Error [Update]
backpropN n as e = fmap snd . foldrM foldBackProp (e, []) $ activsAndLayers 
  where foldBackProp (a,l) (err,us) = second (: us) <$> backpropL l a err
        activsAndLayers = zip as . reverse $ n ^. layers
-}        
{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
addLayer :: Layer -> FFNet -> Either Error FFNet
addLayer = ifDimsMatch doAdd
  where doAdd l = over layers (l <|)
