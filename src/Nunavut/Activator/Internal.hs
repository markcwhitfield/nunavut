{-# LANGUAGE TemplateHaskell #-}
module Nunavut.Activator.Internal where

import Control.Lens (makeLenses, (^.))

import Nunavut.Newtypes (Jacobian)
import Nunavut.Signals (Signal)

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
data ActivatorType = Softmax | Logistic | RectifiedLinear | Linear | Tanh
  deriving (Show, Eq)
data Activator = Activator {
              _activatorType  :: ActivatorType,
              _activatorFunc  :: Signal -> Signal,
              _activatorDeriv :: Signal -> Jacobian
              }
makeLenses ''Activator

{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance Show Activator where
  show                        = show . (^. activatorType)
