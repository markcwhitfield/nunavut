{-# LANGUAGE TemplateHaskell #-}
module Nunavut.Layer(
  Layer(..),
  weights,
  activator,
  filterL
  )where

import Control.Lens (makeLenses)

import Nunavut.Activator
import Nunavut.Filter
import Nunavut.Newtypes

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
data Layer = Layer {
             _weights   :: Weights,
             _activator :: Activator,
             _filterL    :: Filter
             }
makeLenses ''Layer

{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
