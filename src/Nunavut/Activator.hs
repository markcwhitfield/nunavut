{-# LANGUAGE TemplateHaskell #-}
module Nunavut.Activator (
  activatorFunc,
  activatorDeriv,
  logistic,
  relu,
  linear,
  tanhActivator,
  Activator
  ) where

import Control.Lens (makeLenses, (^.))

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
data ActivatorType = Logistic | RectifiedLinear | Linear | Tanh
  deriving (Show, Eq)
data Activator = Activator {
                 _activatorType :: ActivatorType,
                 _activatorFunc :: Double -> Double,
                 _activatorDeriv    :: Double -> Double
                 }
makeLenses ''Activator


{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance Show Activator where
  show = show . (^. activatorType)


{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
logistic :: Activator
logistic = Activator Logistic logisticFunc logisticDeriv

relu :: Activator
relu = Activator RectifiedLinear reluFunc reluDeriv

linear :: Activator
linear = Activator Linear id $ const 1

tanhActivator :: Activator
tanhActivator = Activator Tanh tanh ((1 -) . (** 2) . tan)

logisticFunc :: Double -> Double
logisticFunc z = 1 / (1 - exp (-z))

logisticDeriv :: Double -> Double
logisticDeriv z = s * (1 - s)
  where s = logisticFunc z
  
reluFunc :: Double -> Double
reluFunc z = max z 0

reluDeriv :: Double -> Double
reluDeriv z
  | z <= 0    = 0
  | otherwise = 1
