{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Nunavut.Activator (
  propA,
  backpropA,
  unsafeBackpropA,
  activatorFunc,
  activatorDeriv,
  softmax,
  logistic,
  tanhActivator,
  relu,
  linear,
  Activator
  ) where

import           Control.Lens               ((%=), (^.), _2)
import           Control.Monad.Identity     (Identity, runIdentity)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.RWS    (get, mapRWST)
import           Control.Monad.Writer       (MonadWriter, tell)
import           Data.Monoid                (mempty)
import           Numeric.LinearAlgebra      (diag, maxElement, outer)

import           Nunavut.Activator.Internal (Activator (..), ActivatorType (..),
                                             activatorDeriv, activatorFunc)
import           Nunavut.Newtypes           (Jacobian, elementwise, fromMtx,
                                             fromVec, l1Norm, toVec, trans,
                                             (<>))
import           Nunavut.Propogation        (Backpropogation, PropData,
                                             PropData (..), preActivated,
                                             withBias, withoutBias)
import           Nunavut.Signals            (Signal)
import           Nunavut.Util               (Error, ifDimsMatch)

{--------------------------------------------------------------------------
-                              Propogation                               -
--------------------------------------------------------------------------}
propA :: (Monad m, MonadWriter PropData m)
  => Activator -> Signal -> m Signal
propA a sig = do
  tell $ PData mempty [sig]
  return . withBias $ (a ^. activatorFunc) sig

unsafeBackpropA :: Backpropogation Activator Identity
unsafeBackpropA a err = do
  (_, pData)                                                     <- get
  let jac = (a ^. activatorDeriv) (head $ pData ^. preActivated)
  _2 . preActivated %= tail
  return . (trans jac <>) . withoutBias $ err

backpropA :: Backpropogation Activator (Either Error)
backpropA a err = do
  (_, pData)                                                      <- get
  let preA = head $ pData ^. preActivated
  checkedErr                                                      <- checkErr preA
  mapRWST (return . runIdentity) . unsafeBackpropA a $ checkedErr
    where checkErr preA = lift $ ifDimsMatch "backpropA" (\_ -> const err) preA (withoutBias err)

{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
diagActivator ::
     ActivatorType
  -> (Double -> Double)
  -> (Double -> Double)
  -> Activator
diagActivator t f d                = Activator t (elementwise f) (toDiag d)
  where toDiag d'                  = fromMtx . diag . toVec . elementwise d'

logistic :: Activator
logistic                           = diagActivator Logistic logisticFunc logisticDeriv

relu :: Activator
relu                               = diagActivator RectifiedLinear reluFunc reluDeriv

linear :: Activator
linear                             = diagActivator Linear id $ const 1

tanhActivator :: Activator
tanhActivator                      = diagActivator Tanh tanh ((1 -) . (** 2) . tanh)

logisticFunc :: Double -> Double
logisticFunc z                     = 1 / (1 + exp (-z))

logisticDeriv :: Double -> Double
logisticDeriv z                    = s * (1 - s)
  where s                          = logisticFunc z

reluFunc :: Double -> Double
reluFunc z                         = max z 0

reluDeriv :: Double -> Double
reluDeriv z
  | z <= 0                         = 0
  | otherwise                      = 1

softmax :: Activator
softmax                            = Activator Softmax softmaxFunc softmaxDeriv

softmaxFunc :: Signal -> Signal
softmaxFunc v                      = elementwise (/ l1Norm exponentiated) exponentiated
  where exponentiated              = elementwise (exp . (\e -> e - maxV)) v
        maxV                       = maxElement . toVec $ v

softmaxDeriv :: Signal -> Jacobian
softmaxDeriv v                     = fromMtx $ diag s - (s `outer` s)
  where s                          = toVec . softmaxFunc $ v'
        v'                         = fromVec . toVec $ v
