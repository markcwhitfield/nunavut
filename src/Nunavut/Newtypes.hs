module Nunavut.Newtypes where

import Numeric.LinearAlgebra (Matrix, Vector)

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
newtype Activation = Activ { unActiv :: Vector Double }
newtype ErrorSignal = ErrSig { unErrSig :: Vector Double }
newtype Weights = Weights { unWeights :: Matrix Double }


{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
