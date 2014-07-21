{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Nunavut.Propogation where

import Control.Lens (to)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Identity (IdentityT, runIdentityT)
import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Writer (Writer)
import Numeric.LinearAlgebra (Vector, dim)

import Nunavut.Newtypes
import Nunavut.Util

{--------------------------------------------------------------------------
-                              Typeclasses                               -
--------------------------------------------------------------------------}
class Propogate a where
  unsafePropogate :: a -> Signal -> PropResult IdentityT
  propogate :: a -> Signal -> PropResult (EitherT Error)
  propogate a = lift . runIdentityT . unsafePropogate a

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
newtype Signal = Sig { unSig :: Vector Double }
  deriving (Show, Eq, Ord, Num)
newtype ErrorSignal = ErrSig { unErrSig :: Vector Double }
  deriving (Show, Eq, Ord, Num)

type Activations = [Signal]
type PropResult t = t (Writer Activations) Signal

{--------------------------------------------------------------------------
-                              Constructors                              -
--------------------------------------------------------------------------}
mkSig :: Vector Double -> Signal
mkSig = Sig
mkErrSig :: Vector Double -> ErrorSignal
mkErrSig = ErrSig

{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance SizedOperator Signal where
  outSize = to $ dim . unSig
  inSize = outSize
instance SizedOperator ErrorSignal where
  outSize = to $ dim . unErrSig
  inSize = outSize

instance HasVec Signal where
  toVec = unSig
  fromVec = mkSig
instance HasVec ErrorSignal where
  toVec = unErrSig
  fromVec = mkErrSig
