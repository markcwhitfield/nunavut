{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Nunavut.Propogation where

import Control.Lens (to, Lens', lens)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Identity (IdentityT, runIdentityT)
import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Writer (Writer)
import Data.Monoid (Monoid, mappend, mempty)
import Numeric.LinearAlgebra (Vector, Matrix, dim)

import Nunavut.Newtypes
import Nunavut.Util

{--------------------------------------------------------------------------
-                              Typeclasses                               -
--------------------------------------------------------------------------}
class Propogate a where
  unsafePropogate :: a -> Signal -> PropResult IdentityT
  propogate :: a -> Signal -> PropResult (EitherT Error)
  propogate a = lift . runIdentityT . unsafePropogate a

  unsafeBackprop :: a -> ErrorSignal -> BackpropResult IdentityT
  backprop       :: a -> ErrorSignal -> BackpropResult (EitherT Error)
  backprop a = lift . runIdentityT . unsafeBackprop a

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
data PropData = PData {
                _weighted  :: [Signal],
                _activated :: [Signal],
                _filtered  :: [Signal]
                }


newtype Signal = Sig { unSig :: Vector Double }
  deriving (Show, Eq, Ord, Num)
newtype ErrorSignal = ErrSig { unErrSig :: Vector Double }
  deriving (Show, Eq, Ord, Num)

newtype Update = Update { unUpdate :: Matrix Double }
  deriving (Show, Eq)


type Updates = [Update]
type PropResult t = t (Writer PropData) Signal
type BackpropResult t = t (Writer Updates) ErrorSignal

{--------------------------------------------------------------------------
-                                 Lenses                                 -
--------------------------------------------------------------------------}
weighted :: Lens' PropData [Signal]
weighted = lens _weighted (\p s -> p { _weighted = s })

activated :: Lens' PropData [Signal]
activated = lens _activated (\p s -> p { _activated = s })

filtered :: Lens' PropData [Signal]
filtered = lens _filtered (\p s -> p { _filtered = s })

{--------------------------------------------------------------------------
-                              Constructors                              -
--------------------------------------------------------------------------}
mkSig :: Vector Double -> Signal
mkSig = Sig
mkErrSig :: Vector Double -> ErrorSignal
mkErrSig = ErrSig

mkUpdate :: Matrix Double -> Update
mkUpdate = Update


{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance Monoid PropData where
  mempty = PData mempty mempty mempty
  mappend (PData w1 a1 f1) (PData w2 a2 f2) =
    PData (w1 `mappend` w2) (a1 `mappend` a2) (f1 `mappend` f2)

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

instance HasMtx Update where
  toMtx = unUpdate
  fromMtx = mkUpdate

