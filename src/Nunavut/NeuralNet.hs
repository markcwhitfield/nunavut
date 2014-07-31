{-# LANGUAGE FlexibleContexts #-}
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
import Data.Monoid (mempty)
import Control.Lens (over)
import Control.Monad.RWS (get, tell, put)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Writer (MonadWriter)

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
unsafeBackprop = foldrMWithUpdates unsafeBackpropL
backprop :: FFNet -> ErrorSignal -> BackpropResult (EitherT Error)
backprop = foldrMWithUpdates backpropL

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
foldrMWithUpdates :: (
  Monad m ,
  MonadWriter Updates m,
  MonadState [Update] m) =>
  (Layer -> ErrorSignal -> m ErrorSignal)
  -> FFNet -> ErrorSignal -> m ErrorSignal
foldrMWithUpdates f (FFNet ls) err = do
  result <- foldrM f err ls
  updates <- get
  tell $ Updates updates
  put mempty
  return result


addLayer :: Layer -> FFNet -> Either Error FFNet
addLayer = ifDimsMatch doAdd
  where doAdd l = over layers (l <|)
