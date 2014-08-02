module Nunavut.Layer.Weights where

import Control.Lens (to, (^.), _1, _2, (%=))
import Control.Monad.RWS (tell, get, ask)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (EitherT, hoistEither)
import Control.Monad.Trans.Identity (IdentityT, runIdentityT)
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
unsafePropW :: Weights -> Signal -> PropResult IdentityT
unsafePropW w sig = do
  tell $ PData [sig] mempty
  return $ w <> sig

propW :: Weights -> Signal -> PropResult (EitherT Error)
propW w sig = do
  checkedSig <- hoistEither $ checkDims w sig
  lift . runIdentityT $ unsafePropW w checkedSig

unsafeBackpropW :: Weights -> ErrorSignal -> BackpropResult IdentityT
unsafeBackpropW w err = do
  conf <- ask
  (_, pData) <- get
  let mulRate = mtxElementwise (* conf  ^. learningRate)
  _1 %= (`mappend` [mulRate $ head (pData ^. preWeights) >< err])
  _2 . preWeights %= tail
  return $ trans w <> err

backpropW :: Weights -> ErrorSignal -> BackpropResult (EitherT Error)
backpropW w err = do
  checkedErr <- hoistEither $ checkDims' err w
  lift . runIdentityT $ unsafeBackpropW w checkedErr
