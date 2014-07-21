module Nunavut.Layer.Weights where

import Control.Lens (to)
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
    let withWeights = w <> sig
    tell $ PData [withWeights] mempty mempty
    return withWeights
  
  propogate w sig = do
    checkedSig <- hoistEither $ checkDims w sig
    lift . runIdentityT $ unsafePropogate w checkedSig
