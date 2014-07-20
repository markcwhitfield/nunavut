{-# LANGUAGE TemplateHaskell #-}
module Nunavut.Activator.Internal where

import Control.Lens (makeLenses, (^.))

import Nunavut.Newtypes
import Nunavut.Propogation

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
instance Propogate Activator where
  unsafePropogate a = elementwise $ a ^. activatorFunc
