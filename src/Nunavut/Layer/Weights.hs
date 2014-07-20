module Nunavut.Layer.Weights where

import Control.Lens (to)
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
  unsafePropogate = (<>)
  propogate w = fmap (w <>) . checkDims w 
