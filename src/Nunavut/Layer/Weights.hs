module Nunavut.Layer.Weights where

import Control.Lens (to, (^.), _1, _2, (%=))
import Control.Monad.Trans.RWS (tell, get, ask, mapRWST)
import Control.Monad.Trans (lift)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Monoid (mempty, mappend)
import Numeric.LinearAlgebra (Matrix, rows, cols, randomVector, reshape, RandDist(..), scale)
import System.Random (randomIO)

import Nunavut.Propogation (BackpropResult, PropResult, PropData(..), learningRate, preWeights, (><))
import Nunavut.Newtypes (HasMtx(..), (<>), mtxElementwise, trans)
import Nunavut.Signals (Signal, ErrorSignal)
import Nunavut.Util (SizedOperator(..), Error, checkDims, checkDims')

newtype Weights = Weights { unWeights :: Matrix Double }
  deriving (Show, Eq)

{--------------------------------------------------------------------------
-                              Constructors                              -
--------------------------------------------------------------------------}
mkWeights :: Matrix Double -> Weights
mkWeights = Weights

initWeights :: Int -> Int -> IO Weights
initWeights rs cs = do
  seed <- randomIO
  let cs' = succ cs
  let stdev = fromIntegral cs' ** (-0.5)
  return . fromMtx . reshape cs' . scale stdev . randomVector seed Gaussian $ (rs * cs')

{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance SizedOperator Weights where
  outSize = to $ rows . unWeights
  inSize = to $ cols . unWeights

instance HasMtx Weights where
  toMtx = unWeights
  fromMtx = mkWeights

{--------------------------------------------------------------------------
-                              Propogation                               -
--------------------------------------------------------------------------}
unsafePropW :: Weights -> Signal -> PropResult Identity
unsafePropW w sig = do
  tell $ PData [sig] mempty
  return $ w <> sig

propW :: Weights -> Signal -> PropResult (Either Error)
propW w sig = do
  checkedSig <- lift $ checkDims "propW" sig w
  mapRWST (return . runIdentity) . unsafePropW w $ checkedSig

unsafeBackpropW :: Weights -> ErrorSignal -> BackpropResult Identity
unsafeBackpropW w err = do
  conf <- ask
  (_, pData) <- get
  let mulRate = mtxElementwise (* conf  ^. learningRate)
  _1 %= (`mappend` [mulRate . trans $ head (pData ^. preWeights) >< err])
  _2 . preWeights %= tail
  return $ trans w <> err

backpropW :: Weights -> ErrorSignal -> BackpropResult (Either Error)
backpropW w err = do
   checkedErr <- lift $ checkDims' "backpropW" w err
   mapRWST (return . runIdentity) . unsafeBackpropW w $ checkedErr

shape :: Weights -> (Int, Int)
shape (Weights w) = (rows w, cols w)
