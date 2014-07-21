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
                _preWeights  :: [Signal],
                _preActivated :: [Signal],
                _preFiltered  :: [Signal]
                }
data PropDatum = PDatum {
                 _dPreWeighted  :: Signal,
                 _dPreActivated :: Signal,
                 _dPreFiltered  :: Signal
                 }


newtype Signal = Sig { unSig :: Vector Double }
  deriving (Show, Eq, Ord, Num)
newtype ErrorSignal = ErrSig { unErrSig :: Vector Double }
  deriving (Show, Eq, Ord, Num)

newtype Update = Update { unUpdate :: Matrix Double }
  deriving (Show, Eq)


type Updates = [Update]
type PropResult t = t (Writer PropData) Signal
type BackpropResult t = t (ReaderT PropDatum (Writer Updates)) ErrorSignal

{--------------------------------------------------------------------------
-                                 Lenses                                 -
--------------------------------------------------------------------------}
preWeights :: Lens' PropData [Signal]
preWeights = lens _preWeights (\p s -> p { _preWeights = s })

preActivated :: Lens' PropData [Signal]
preActivated = lens _preActivated (\p s -> p { _preActivated = s })

preFiltered :: Lens' PropData [Signal]
preFiltered = lens _preFiltered (\p s -> p { _preFiltered = s })

dPreWeighted :: Lens' PropDatum Signal
dPreWeighted = lens _dPreWeighted (\p s -> p { _dPreWeighted = s })

dPreActivated :: Lens' PropDatum Signal
dPreActivated = lens _dPreActivated (\p s -> p { _dPreActivated = s })

dPreFiltered :: Lens' PropDatum Signal
dPreFiltered = lens _dPreFiltered (\p s -> p { _dPreFiltered = s })


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

