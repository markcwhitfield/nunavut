module Nunavut.Newtypes.Internal where

import Numeric.LinearAlgebra (NormType(..))

data Norm = L1 | L2 | InfNorm | Frob
  deriving (Show, Eq)

toNormType :: Norm -> NormType
toNormType InfNorm = Infinity
toNormType L1 = PNorm1
toNormType L2 = PNorm2
toNormType Frob = Frobenius
