{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Nunavut.Newtypes (
  Activation,
  ErrorSignal,
  Input,
  Weights,
  Jacobian,
  Update,
  mkActiv,
  mkErrSig,
  mkInput,
  mkWeights,
  mkJacob,
  mkUpdate,
  unActiv,
  unErrSig,
  unInput,
  unWeights,
  unJacob,
  unUpdate,
  l1Norm,
  l2Norm,
  infNorm,
  frobNorm,
  elementwise,
  Activations,
  HasVec(..),
  HasMtx(..),
  (<>),
  trans,
  outer,
  ) where

import Control.Lens (to)
import Data.List.NonEmpty (NonEmpty)
import Numeric.LinearAlgebra (
  Matrix, Vector, dim, cols, rows, pnorm, NormType(..),
  mapVector)
import qualified Numeric.LinearAlgebra as LA

import Nunavut.Util.Dimensions

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
newtype Activation = Activ { unActiv :: Vector Double }
  deriving (Show, Eq, Ord)
newtype ErrorSignal = ErrSig { unErrSig :: Vector Double }
  deriving (Show, Eq, Ord)
newtype Input = Input { unInput :: Vector Double }
  deriving (Show, Eq, Ord)

newtype Weights = Weights { unWeights :: Matrix Double }
  deriving (Show, Eq)
newtype Jacobian = Jacob { unJacob :: Matrix Double }
  deriving (Show, Eq)
newtype Update = Update { unUpdate :: Matrix Double }
  deriving (Show, Eq)

type Activations = NonEmpty Activation

data Norm = L1 | L2 | InfNorm | Frob

{--------------------------------------------------------------------------
-                                Classes                                 -
--------------------------------------------------------------------------}
class HasNorm a where
  pNorm :: Norm -> a -> Double
class HasVec a where
  toVec :: a -> Vector Double
  fromVec :: Vector Double -> a
class HasMtx a where
  toMtx :: a -> Matrix Double
  fromMtx :: Matrix Double -> a
class Mul a b c | a b -> c where
  (<>) :: a -> b -> c


{--------------------------------------------------------------------------
-                              Contructors                               -
--------------------------------------------------------------------------}
mkActiv :: Vector Double -> Activation
mkActiv = Activ
mkErrSig :: Vector Double -> ErrorSignal
mkErrSig = ErrSig
mkInput :: Vector Double -> Input
mkInput = Input

mkWeights :: Matrix Double -> Weights
mkWeights = Weights
mkJacob :: Matrix Double -> Jacobian
mkJacob = Jacob
mkUpdate :: Matrix Double -> Update
mkUpdate = Update


{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance SizedOperator Weights where
  outSize = to $ rows . unWeights
  inSize = to $ cols . unWeights

instance SizedOperator Jacobian where
  outSize = to $ rows . unJacob
  inSize = to $ cols . unJacob

instance SizedOperator Activation where
  outSize = to $ dim . unActiv
  inSize = outSize

instance SizedOperator ErrorSignal where
  outSize = to $ dim . unErrSig
  inSize = outSize

instance HasVec Activation where
  toVec = unActiv
  fromVec = mkActiv
instance HasVec ErrorSignal where
  toVec = unErrSig
  fromVec = mkErrSig
instance HasVec Input where
  toVec = unInput
  fromVec = mkInput

instance HasMtx Weights where
  toMtx = unWeights
  fromMtx = mkWeights
instance HasMtx Jacobian where
  toMtx = unJacob
  fromMtx = mkJacob
instance HasMtx Update where
  toMtx = unUpdate
  fromMtx = mkUpdate


instance HasNorm Activation where
  pNorm norm a = pnorm (toNormType norm) (unActiv a)
instance HasNorm ErrorSignal where
  pNorm norm e = pnorm (toNormType norm) (unErrSig e)

instance (HasMtx a, HasVec b) => Mul a b b where
  a <> b = fromVec $ toMtx a LA.<> toVec b
instance (HasMtx a) => Mul a a a where
  a <> b = fromMtx $ toMtx a LA.<> toMtx b
instance (HasVec a) => Mul a a Double where
  a <> b = toVec a LA.<.> toVec b

{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
wrapM :: (HasMtx a) => (Matrix Double -> Matrix Double) -> a -> a
wrapM f = fromMtx . f . toMtx

trans :: (HasMtx a) => a -> a
trans = wrapM LA.trans

outer :: Activation -> ErrorSignal -> Update
outer (Activ a) (ErrSig b) = mkUpdate (a `LA.outer` b)

l1Norm :: (HasNorm a) => a -> Double
l1Norm = pNorm L1

l2Norm :: (HasNorm a) => a -> Double
l2Norm = pNorm L2

infNorm :: (HasNorm a) => a -> Double
infNorm = pNorm InfNorm

frobNorm :: (HasNorm a) => a -> Double
frobNorm = pNorm Frob

elementwise :: (HasVec a) => (Double -> Double) -> a -> a
elementwise f = fromVec . mapVector f . toVec


toNormType :: Norm -> NormType
toNormType InfNorm = Infinity
toNormType L1 = PNorm1
toNormType L2 = PNorm2
toNormType Frob = Frobenius
