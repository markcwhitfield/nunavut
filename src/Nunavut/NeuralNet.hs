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
import           Nunavut.NeuralNet.Internal

import           Prelude                    hiding (concat)

import           Control.Applicative        ((<$>), (<*>))
import           Control.Lens               (over, (.=), (^.), _1)
import           Control.Monad              (join, liftM2)
import           Control.Monad.Identity     (Identity)
import           Control.Monad.Trans.RWS    (evalRWST, get, tell)
import           Data.Foldable              (foldrM)
import           Data.List.NonEmpty         (NonEmpty (..), fromList, (<|))
import qualified Data.List.NonEmpty         as NE
import           Data.List.Split            (chunksOf)
import           Data.Monoid                (mappend, mempty)

import           Nunavut.Activator          (Activator)
import           Nunavut.ErrorFunction      (getErrSig)
import           Nunavut.Layer              (Layer, backpropL, initLayer, propL,
                                             unsafeBackpropL, unsafePropL,
                                             weights)
import           Nunavut.Newtypes           (HasVec (..), Input, Label, shape,
                                             shapeMismatch, wrapM)
import           Nunavut.Propogation        (BackpropResult, Backpropogation,
                                             PropConfig (..), PropData (..),
                                             Propogation, Update (..),
                                             Updates (..), batchSize, errFunc,
                                             withBias, withoutBias)
import           Nunavut.Signals            (ErrorSignal, Signal)
import           Nunavut.Util               (Error, checkDims', lengthMismatch)

{--------------------------------------------------------------------------
-                              Constructors                              -
--------------------------------------------------------------------------}
oneLayer :: Layer -> FFNet
oneLayer l                  = FFNet (l :| [])

mkFFNet ::
     Activator
  -> (Int, Int)
  -> [Int]
  -> IO FFNet
mkFFNet a (out,inp) []      = oneLayer <$> initLayer a out inp
mkFFNet a (out,inp) (l:ls)  = unsafeAddLayer <$> initLayer a out l <*> go (l:|ls)
  where go (out':|[])       = oneLayer <$> initLayer a out' inp
        go (out':|inp':ls') = unsafeAddLayer <$> initLayer a out' inp' <*> go (inp':|ls')

{--------------------------------------------------------------------------
-                              Propogation                               -
--------------------------------------------------------------------------}
unsafePropogate :: Propogation FFNet Identity
unsafePropogate (FFNet ls) sig = fmap withoutBias . foldrM unsafePropL (withBias sig) $ ls
propogate :: Propogation FFNet (Either Error)
propogate (FFNet ls) sig = fmap withoutBias . foldrM propL (withBias sig) $ ls

unsafeBackprop :: Backpropogation FFNet Identity
unsafeBackprop net = foldrMWithUpdates unsafeBackpropL net . withBias
backprop :: Backpropogation FFNet (Either Error)
backprop net = foldrMWithUpdates backpropL net . withBias

predict :: FFNet -> Input -> Either Error Signal
predict n = fmap fst . (\m -> evalRWST m () ()) . propogate n . fromVec . toVec

train ::
     PropConfig
  -> FFNet
  -> [(Input, Label)]
  -> Either Error FFNet
train conf net dat                          = foldrM trainBatch net (chunksOf bSize dat)
  where bSize                               = conf ^. batchSize
        trainBatch batch net'               = updateWeights net' =<< foldrM trainAndUpdate mempty batch
         where trainAndUpdate datum updates = (updates `mappend`) <$> train1 conf net' datum

train1 ::
     PropConfig
  -> FFNet
  -> (Input, Label)
  -> Either Error Updates
train1 conf net (inp,lbl') = case checkDims' "train1" net lbl' of
  Left err  -> Left err
  Right lbl -> unsafeTrain1 conf net (inp,lbl)

unsafeTrain1 ::
     PropConfig
  -> FFNet
  -> (Input, Label)
  -> Either Error Updates
unsafeTrain1 conf net (inp,lbl) = do
    let run r s m = evalRWST m r s

    (sig, PData preW preA) <- run () () $ propogate net (fromVec . toVec $ inp)
    let err   = (conf ^. errFunc . getErrSig) lbl sig
        pData = PData (reverse preW) (reverse preA)

    (_, Updates updates) <- run conf ([], pData) $ backprop net err
    return . Updates . reverse $ updates

{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
foldrMWithUpdates :: Monad m =>
  (Layer -> ErrorSignal -> BackpropResult m)
  -> FFNet -> ErrorSignal -> BackpropResult m
foldrMWithUpdates f (FFNet ls) err = do
  result       <- foldrM f err $ NE.reverse ls
  (updates, _) <- get
  tell . Updates . reverse $ updates
  _1 .= mempty
  return result

addLayer :: Layer -> FFNet -> Either Error FFNet
addLayer l' net@(FFNet (l:|_)) = flip unsafeAddLayer net <$> checkDims' "addLayer" l l'

unsafeAddLayer :: Layer -> FFNet -> FFNet
unsafeAddLayer l = over layers (l <|)

updateWeights :: FFNet -> Updates -> Either Error FFNet
updateWeights net (Updates uus)        = go lls uus
  where lls                            = net ^. layers
        go (l:|[]) (Update u:[])       = Right . oneLayer =<< updateWeight l u
        go (l:|ls@(_:_)) (Update u:us) = join . liftM2 addLayer (updateWeight l u) $ go (fromList ls) us
        go _ _                         = Left . lengthMismatch "updateWeights" lls $ uus
        updateWeight l u               = if shape (l ^. weights) == shape u
                                         then Right $ over weights (wrapM (+ u)) l
                                         else Left . shapeMismatch "updateWeights" (l ^. weights) $ u
