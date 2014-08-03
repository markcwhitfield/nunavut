{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Nunavut.Propogation where

import Control.Lens (Lens', lens)
import Control.Monad.Trans.RWS (RWST)
import Data.Monoid (Monoid, mappend, mempty)
import Numeric.LinearAlgebra (Matrix, outer, fromList, toList)

import Nunavut.ErrorFunction (ErrorFunction)
import Nunavut.Newtypes (HasMtx(..), HasVec(..))
import Nunavut.Signals (Signal(..), ErrorSignal(..))

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
data PropConfig = PConfig { 
                  _learningRate :: Double,
                  _batchSize    :: Int,
                  _errFunc      :: ErrorFunction
                  } deriving (Show)
data PropData = PData {
                _preWeights   :: [Signal],
                _preActivated :: [Signal]
                } deriving (Show, Eq)

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

errFunc :: Lens' PropConfig ErrorFunction
errFunc = lens _errFunc (\c r -> c { _errFunc = r })

preWeights :: Lens' PropData [Signal]
preWeights = lens _preWeights (\p s -> p { _preWeights = s })

preActivated :: Lens' PropData [Signal]
preActivated = lens _preActivated (\p s -> p { _preActivated = s })


{--------------------------------------------------------------------------
-                              Constructors                              -
--------------------------------------------------------------------------}
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

instance HasMtx Update where
  toMtx = unUpdate
  fromMtx = mkUpdate

{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
onElements :: HasVec a => ([Double] -> [Double]) -> a -> a
onElements f = fromVec . fromList . f . toList . toVec

withBias :: HasVec a => a -> a
withBias = onElements (1 :)

withoutBias :: HasVec a => a -> a
withoutBias = onElements tail

(><) :: Signal -> ErrorSignal -> Update
(><) (Sig sig) (ErrSig err) = fromMtx $ sig `outer` err
