module Nunavut.Layer.Weights where

import Control.Lens (to)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (hoistEither)
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
  unsafePropogate w = lift . return . (w <>)
  propogate w sig = do
    eSig <- hoistEither $ checkDims w sig
    return $ w <> eSig
