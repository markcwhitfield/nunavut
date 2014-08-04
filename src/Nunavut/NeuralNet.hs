{-# LANGUAGE FlexibleContexts #-}
module Nunavut.NeuralNet (
  unsafePropogate,
  unsafeBackprop,
  propogate,
  backprop,
  predict,
  train,
  FFNet,
  oneLayer,
  mkFFNet,
  layers,
  updateWeights,
  addLayer) where 
import Nunavut.NeuralNet.Internal

import Prelude hiding (reverse, concat)

import Control.Applicative ((<$>), (<*>))
import Control.Lens (over, (.=), _1, (^.))
import Control.Monad.Trans.RWS (get, tell, evalRWST)
import Control.Monad.Identity (Identity)
import Data.Foldable (foldrM)
import Data.List.NonEmpty (NonEmpty(..), reverse, (<|), fromList, toList)
import Data.List.Split (chunksOf)
import Data.Monoid (mempty, mappend)
import Data.Text.Lazy (pack, concat)

import Nunavut.ErrorFunction (getErrSig)
import Nunavut.Activator (Activator)
import Nunavut.Layer (Layer, initLayer, propL, backpropL, unsafePropL, unsafeBackpropL, weights)
import Nunavut.Newtypes (Input, HasVec(..), Label, wrapM) 
import Nunavut.Propogation (PropResult, BackpropResult, Update(..), Updates(..), withBias, withoutBias, PropConfig(..), batchSize, errFunc)
import Nunavut.Signals (Signal, ErrorSignal)
import Nunavut.Util (Error, mkError, ifDimsMatch)

{--------------------------------------------------------------------------
-                              Constructors                              -
--------------------------------------------------------------------------}
oneLayer :: Layer -> FFNet
oneLayer l = FFNet (l :| [])

mkFFNet :: Activator -> (Int, Int) -> [Int] -> IO FFNet
mkFFNet a (out,inp) [] = oneLayer <$> initLayer a out inp
mkFFNet a (out,inp) (l:ls) = unsafeAddLayer <$> initLayer a out l <*> go (l:|ls)
  where go (out':|[]) = oneLayer <$> initLayer a out' inp
        go (out':|inp':ls') = unsafeAddLayer <$> initLayer a out' inp' <*> go (inp':|ls')

{--------------------------------------------------------------------------
-                              Propogation                               -
--------------------------------------------------------------------------}
unsafePropogate :: FFNet -> Signal -> PropResult Identity
unsafePropogate (FFNet ls) sig = fmap withoutBias . foldrM unsafePropL (withBias sig) $ ls
propogate :: FFNet -> Signal -> PropResult (Either Error)
propogate (FFNet ls) sig = fmap withoutBias . foldrM propL (withBias sig) $ ls

unsafeBackprop :: FFNet -> ErrorSignal -> BackpropResult Identity
unsafeBackprop = foldrMWithUpdates unsafeBackpropL
backprop :: FFNet -> ErrorSignal -> BackpropResult (Either Error)
backprop = foldrMWithUpdates backpropL

predict :: FFNet -> Input -> Either Error Signal
predict n = fmap fst . (\m -> evalRWST m () ()) . propogate n . fromVec . toVec

train :: PropConfig -> FFNet -> [(Input, Label)] -> Either Error FFNet
train conf net dat = foldrM trainBatch net (chunksOf bSize dat)
  where bSize = conf ^. batchSize
        trainBatch batch net' = updateWeights net' =<< foldrM trainAndUpdate mempty batch
         where trainAndUpdate datum updates = (updates `mappend`) <$> train1 conf net' datum

train1 :: PropConfig -> FFNet -> (Input, Label) -> Either Error Updates
train1 conf net (inp, lbl) = do
  let run r s m = evalRWST m r s
  (sig, pData) <- run () () $ propogate net (fromVec . toVec $ inp)
  let err = (conf ^. errFunc . getErrSig) lbl sig
  (_, updates) <- run conf ([], pData) $ backprop net err
  return updates

{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
foldrMWithUpdates :: Monad m =>
  (Layer -> ErrorSignal -> BackpropResult m)
  -> FFNet -> ErrorSignal -> BackpropResult m
foldrMWithUpdates f (FFNet ls) err = do
  result <- foldrM f err $ reverse ls
  (updates, _) <- get
  tell $ Updates updates
  _1 .= mempty
  return result

addLayer :: Layer -> FFNet -> Either Error FFNet
addLayer = ifDimsMatch unsafeAddLayer

unsafeAddLayer :: Layer -> FFNet -> FFNet
unsafeAddLayer l = over layers (l <|)

updateWeights :: FFNet -> Updates -> Either Error FFNet
updateWeights (FFNet lls) (Updates uus) = go lls uus
  where go (l :| []) (Update u : []) = Right . oneLayer $ updateWeight l u
        go (l :| ls) (Update u : us) = addLayer (updateWeight l u) =<< go (fromList ls) us
        go _ _ = Left . mkError . concat $ [
                    "Dimension Mismatch: ",
                    "FFNet length ", pack . show . length . toList $ lls,
                    "Does not match updates length ", pack . show . length $ uus]
        updateWeight l u = over weights (wrapM (+ u)) l
