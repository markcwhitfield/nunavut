{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Nunavut.Signals where

import           Control.Lens          (to)
import           Numeric.LinearAlgebra (Vector, dim, zipVectorWith)

import           Nunavut.Newtypes      (HasVec (..))
import           Nunavut.Util          (SizedOperator (..))

newtype Signal = Sig { unSig            :: Vector Double }
  deriving (Show, Eq, Ord, Num)
newtype ErrorSignal = ErrSig { unErrSig :: Vector Double }
  deriving (Show, Eq, Ord, Num)


mkSig :: Vector Double -> Signal
mkSig                                    = Sig
mkErrSig :: Vector Double -> ErrorSignal
mkErrSig                                 = ErrSig



instance SizedOperator Signal where
  outSize                                = to $ dim . unSig
  inSize                                 = outSize
instance SizedOperator ErrorSignal where
  outSize                                = to $ dim . unErrSig
  inSize                                 = outSize

instance HasVec Signal where
  toVec                                  = unSig
  fromVec                                = mkSig
instance HasVec ErrorSignal where
  toVec                                  = unErrSig
  fromVec                                = mkErrSig

(.*) :: ErrorSignal -> Signal -> ErrorSignal
(.*) (ErrSig err) (Sig sig) = fromVec $ zipVectorWith (*) err sig
