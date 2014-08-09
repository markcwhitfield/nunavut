module Nunavut.Layer.Weights where

import           Control.Lens            (to, (%=), (^.), _1, _2)
import           Control.Monad.Identity  (Identity, runIdentity)
import           Control.Monad.Trans     (lift)
import           Control.Monad.Trans.RWS (ask, get, mapRWST, tell)
import           Data.Monoid             (mappend, mempty)
import           Numeric.LinearAlgebra   (Matrix, RandDist (..), cols,
                                          randomVector, reshape, rows, scale)
import           System.Random           (randomIO)

import           Nunavut.Newtypes        (HasMtx (..), mtxElementwise, trans,
                                          (<>))
import           Nunavut.Propogation     (Backpropogation, PropData (..),
                                          Propogation, learningRate, preWeights,
                                          (><))
import           Nunavut.Util            (Error, SizedOperator (..), checkDims,
                                          checkDims')

newtype Weights = Weights { unWeights :: Matrix Double }
  deriving (Show, Eq)

{--------------------------------------------------------------------------
-                              Constructors                              -
--------------------------------------------------------------------------}
mkWeights :: Matrix Double -> Weights
mkWeights = Weights

initWeights :: Int -> Int -> IO Weights
initWeights rs cs = do
  seed <- randomIO
  let cs'   = succ cs
      stdev = fromIntegral cs' ** (-0.5)
  return . fromMtx . reshape cs' . scale stdev . randomVector seed Gaussian $ (rs * cs')

{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance SizedOperator Weights where
  outSize                            = to $ rows . unWeights
  inSize                             = to $ cols . unWeights

instance HasMtx Weights where
  toMtx                              = unWeights
  fromMtx                            = mkWeights

{--------------------------------------------------------------------------
-                              Propogation                               -
--------------------------------------------------------------------------}
unsafePropW :: Propogation Weights Identity
unsafePropW w sig = do
  tell $ PData [sig] mempty
  return $ w <> sig

propW :: Propogation Weights (Either Error)
propW w sig = do
  checkedSig <- lift $ checkDims "propW" sig w
  mapRWST (return . runIdentity) . unsafePropW w $ checkedSig

unsafeBackpropW :: Backpropogation Weights Identity
unsafeBackpropW w err = do
  conf       <- ask
  (_, pData) <- get
  let mulRate = mtxElementwise (* conf  ^. learningRate)
  _1 %= (`mappend` [mulRate . trans $ head (pData ^. preWeights) >< err])
  _2 . preWeights %= tail
  return $ trans w <> err

backpropW :: Backpropogation Weights (Either Error)
backpropW w err = do
   checkedErr <- lift $ checkDims' "backpropW" w err
   mapRWST (return . runIdentity) . unsafeBackpropW w $ checkedErr

shape :: Weights -> (Int, Int)
shape (Weights w) = (rows w, cols w)
