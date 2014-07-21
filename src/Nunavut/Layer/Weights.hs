module Nunavut.Layer.Weights where

import Control.Lens (to, (^.))
import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (hoistEither)
import Control.Monad.Trans.Identity (runIdentityT)
import Control.Monad.Writer (tell)
import Data.Monoid (mempty)
import Numeric.LinearAlgebra (Matrix, rows, cols)

import Nunavut.Propogation
import Nunavut.Newtypes
import Nunavut.Util

newtype Weights = Weights { unWeights :: Matrix Double }
  deriving (Show, Eq)

mkWeights :: Matrix Double -> Weights
mkWeights = Weights

instance SizedOperator Weights where
  outSize = to $ rows . unWeights
  inSize = to $ cols . unWeights

instance HasMtx Weights where
  toMtx = unWeights
  fromMtx = mkWeights

instance Propogate Weights where
  unsafePropogate w sig = do
    tell $ PData [sig] mempty mempty
    return $ w <> sig
  
  propogate w sig = do
    checkedSig <- hoistEither $ checkDims w sig
    lift . runIdentityT $ unsafePropogate w checkedSig

  unsafeBackprop w err = do
    datum <- ask
    tell [(datum ^. dPreWeighted) >< err]
    return $ trans w <> err

  backprop w err = do
    checkedErr <- hoistEither $ checkDims' err w
    lift . runIdentityT $ unsafeBackprop w checkedErr
