{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Nunavut.Propogation where

import Control.Lens (to, Lens', lens)
import Control.Monad.RWS (RWS)
import Data.Monoid (Monoid, mappend, mempty)
import Numeric.LinearAlgebra (Vector, Matrix, dim, zipVectorWith, outer,
  fromList, toList)

import Nunavut.Newtypes
import Nunavut.Util

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
data PropConfig = PConfig { 
                  _learningRate :: Double,
                  _batchSize    :: Int
                  } deriving (Eq, Show)
data PropData = PData {
                _preWeights   :: [Signal],
                _preActivated :: [Signal]
                } deriving (Show, Eq)

newtype Signal = Sig { unSig :: Vector Double }
  deriving (Show, Eq, Ord, Num)
newtype ErrorSignal = ErrSig { unErrSig :: Vector Double }
  deriving (Show, Eq, Ord, Num)

newtype Update = Update { unUpdate :: Matrix Double }
  deriving (Show, Eq)


newtype Updates = Updates { unUpdates :: [Update] } deriving (Show)
type PropResult m = RWST () PropData () m Signal
type BackpropResult m = RWST PropConfig Updates ([Update], PropData) m ErrorSignal

{--------------------------------------------------------------------------
-                                 Lenses                                 -
--------------------------------------------------------------------------}
learningRate :: Lens' PropConfig Double
learningRate = lens _learningRate (\c r -> c { _learningRate = r })

batchSize :: Lens' PropConfig Int
batchSize = lens _batchSize (\c r -> c { _batchSize = r })

preWeights :: Lens' PropData [Signal]
preWeights = lens _preWeights (\p s -> p { _preWeights = s })

preActivated :: Lens' PropData [Signal]
preActivated = lens _preActivated (\p s -> p { _preActivated = s })


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
  mempty = PData mempty mempty
  mappend (PData w1 a1) (PData w2 a2) =
    PData (w1 `mappend` w2) (a1 `mappend` a2)
instance Monoid Updates where
  mempty = Updates []
  (Updates (u1:u1s)) `mappend` (Updates (u2:u2s)) = Updates $ u3 : (u1s `mappend` u2s)
    where u3 = fromMtx $ toMtx u1 + toMtx u2
  (Updates []) `mappend` u2s = u2s
  u1s `mappend` (Updates []) = u1s
            

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

{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
onElements :: HasVec a => ([Double] -> [Double]) -> a -> a
onElements f = fromVec . fromList . f . toList . toVec

withBias :: Signal -> Signal
withBias = onElements (1 :)

withoutBias :: ErrorSignal -> ErrorSignal
withoutBias = onElements tail

(.*) :: ErrorSignal -> Signal -> ErrorSignal
(.*) (ErrSig err) (Sig sig) = fromVec $ zipVectorWith (*) err sig

(><) :: Signal -> ErrorSignal -> Update
(><) (Sig sig) (ErrSig err) = fromMtx $ sig `outer` err
