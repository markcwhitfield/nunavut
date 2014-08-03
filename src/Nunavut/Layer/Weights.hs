module Nunavut.Layer.Weights where

import Control.Lens (to, (^.), _1, _2, (%=))
import Control.Monad.Trans.RWS (tell, get, ask, mapRWST)
import Control.Monad.Trans (lift)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Monoid (mempty, mappend)
import Numeric.LinearAlgebra (Matrix, rows, cols)

import Nunavut.Propogation
import Nunavut.Newtypes
import Nunavut.Util

newtype Weights = Weights { unWeights :: Matrix Double }
  deriving (Show, Eq)

mkWeights :: Matrix Double -> Weights
mkWeights = Weights

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
  checkedSig <- lift $ checkDims w sig
  mapRWST (return . runIdentity) . unsafePropW w $ checkedSig

unsafeBackpropW :: Weights -> ErrorSignal -> BackpropResult Identity
unsafeBackpropW w err = do
  conf <- ask
  (_, pData) <- get
  let mulRate = mtxElementwise (* conf  ^. learningRate)
  _1 %= (`mappend` [mulRate $ head (pData ^. preWeights) >< err])
  _2 . preWeights %= tail
  return $ trans w <> err

backpropW :: Weights -> ErrorSignal -> BackpropResult (Either Error)
backpropW w err = do
   checkedErr <- lift $ checkDims' err w
   mapRWST (return . runIdentity) . unsafeBackpropW w $ checkedErr

shape :: Weights -> (Int, Int)
shape (Weights w) = (rows w, cols w)
