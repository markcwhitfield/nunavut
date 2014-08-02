{-# LANGUAGE FlexibleContexts #-}
module Nunavut.NeuralNet (
  unsafePropogate,
  unsafeBackprop,
  propogate,
  backprop,
  predict,
  FFNet,
  oneLayer,
  mkFFNet,
  layers,
  updateWeights,
  addLayer) where 
import Nunavut.NeuralNet.Internal

import Prelude hiding (reverse, concat)

import Data.Foldable (foldrM)
import Data.List.NonEmpty (NonEmpty(..), reverse, (<|), fromList, toList)
import Data.Monoid (mempty)
import Data.Text.Lazy (pack, concat)
import Control.Lens (over, (.=), _1)
import Control.Monad.RWS (get, tell, evalRWS)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Writer (MonadWriter)

import Nunavut.Layer
import Nunavut.Newtypes
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

predict :: PropConfig -> FFNet -> Signal -> Either Error Signal
predict p n = fst . (\m -> evalRWS m p ()) . runEitherT . propogate n

{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
foldrMWithUpdates :: (
  Monad m ,
  MonadWriter Updates m,
  MonadState ([Update], PropData) m) =>
  (Layer -> ErrorSignal -> m ErrorSignal)
  -> FFNet -> ErrorSignal -> m ErrorSignal
foldrMWithUpdates f (FFNet ls) err = do
  result <- foldrM f err ls
  (updates, _) <- get
  tell $ Updates updates
  _1 .= mempty
  return result

addLayer :: Layer -> FFNet -> Either Error FFNet
addLayer = ifDimsMatch doAdd
  where doAdd l = over layers (l <|)

updateWeights :: FFNet -> Updates -> Either Error FFNet
updateWeights (FFNet lls) (Updates uus) = go lls uus
  where go (l :| []) (Update u : []) = Right . oneLayer $ updateWeight l u
        go (l :| ls) (Update u : us) = addLayer (updateWeight l u) =<< go (fromList ls) us
        go _ _ = Left . mkError . concat $ [
                    "Dimension Mismatch: ",
                    "FFNet length ", pack . show . length . toList $ lls,
                    "Does not match updates length ", pack . show . length $ uus]
        updateWeight l u = over weights (wrapM (+ u)) l
