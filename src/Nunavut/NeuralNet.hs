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

import Prelude hiding (concat)

import Control.Applicative ((<$>), (<*>))
import Control.Lens (over, (.=), _1, (^.))
import Control.Monad.Trans.RWS (get, tell, evalRWST)
import Control.Monad.Identity (Identity)
import Data.Foldable (foldrM)
import Data.List.NonEmpty (NonEmpty(..), (<|), fromList, toList)
import qualified Data.List.NonEmpty as NE
import Data.List.Split (chunksOf)
import Data.Monoid (mempty, mappend)
import Data.Text.Lazy (pack, concat)

import Nunavut.ErrorFunction (getErrSig)
import Nunavut.Activator (Activator)
import Nunavut.Layer (Layer, initLayer, propL, backpropL, unsafePropL, unsafeBackpropL, weights)
import Nunavut.Newtypes (Input, HasVec(..), Label, wrapM, shape) 
import Nunavut.Propogation (PropResult, BackpropResult, Update(..), Updates(..), withBias, withoutBias, PropConfig(..), batchSize, errFunc, PropData(..))
import Nunavut.Signals (Signal, ErrorSignal)
import Nunavut.Util (Error, mkError, checkDims')

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
unsafeBackprop net = foldrMWithUpdates unsafeBackpropL net . withBias
backprop :: FFNet -> ErrorSignal -> BackpropResult (Either Error)
backprop net = foldrMWithUpdates backpropL net . withBias

predict :: FFNet -> Input -> Either Error Signal
predict n = fmap fst . (\m -> evalRWST m () ()) . propogate n . fromVec . toVec

train :: PropConfig -> FFNet -> [(Input, Label)] -> Either Error FFNet
train conf net dat = foldrM trainBatch net (chunksOf bSize dat)
  where bSize = conf ^. batchSize
        trainBatch batch net' = updateWeights net' =<< foldrM trainAndUpdate mempty batch
         where trainAndUpdate datum updates = (updates `mappend`) <$> train1 conf net' datum

train1 :: PropConfig -> FFNet -> (Input, Label) -> Either Error Updates
train1 conf net (inp,lbl') = case checkDims' "train1" net lbl' of
  Right lbl -> do
    let run r s m = evalRWST m r s
    (sig, PData preW preA) <- run () () $ propogate net (fromVec . toVec $ inp)
    let err = (conf ^. errFunc . getErrSig) lbl sig
        pData = PData (reverse preW) (reverse preA)
    (_, Updates updates) <- run conf ([], pData) $ backprop net err
    return . Updates . reverse $ updates
  Left err -> Left err

{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
foldrMWithUpdates :: Monad m =>
  (Layer -> ErrorSignal -> BackpropResult m)
  -> FFNet -> ErrorSignal -> BackpropResult m
foldrMWithUpdates f (FFNet ls) err = do
  result <- foldrM f err $ NE.reverse ls
  (updates, _) <- get
  tell . Updates . reverse $ updates
  _1 .= mempty
  return result

addLayer :: Layer -> FFNet -> Either Error FFNet
addLayer l' net@(FFNet (l:|_)) = flip unsafeAddLayer net <$> checkDims' "addLayer" l l'

unsafeAddLayer :: Layer -> FFNet -> FFNet
unsafeAddLayer l = over layers (l <|)

updateWeights :: FFNet -> Updates -> Either Error FFNet
updateWeights (FFNet lls) (Updates uus) = go lls uus
  where go (l :| []) (Update u : []) = Right . oneLayer =<< updateWeight l u
        go (l :| ls@(_:_)) (Update u : us) = do
          l' <- updateWeight l u
          net <- go (fromList ls) us
          addLayer l' net
        go _ _ = Left . mkError . concat $ [
                    "UPDATEWEIGHTS: Dimension Mismatch: ",
                    "FFNet length ", pack . show . length . toList $ lls,
                    " does not match updates length ", pack . show . length $ uus]
        updateWeight l u = if shape (l ^. weights) == shape u 
                           then Right $ over weights (wrapM (+ u)) l 
                           else Left . mkError . concat $ [
                             "UPDATEWEIGHTS: Dimension Mismatch: ",
                             "Weight matrix shape ", pack . show . shape $ (l ^. weights),
                             " does not match update matrix shape ", pack . show . shape $ u]
