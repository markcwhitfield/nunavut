{-# LANGUAGE TemplateHaskell #-}
module Nunavut.Layer(
  Layer(..),
  weights,
  activator,
  filterL,
  inSize,
  outSize
  )where

import Control.Lens (makeLenses)

import Nunavut.Activator
import Nunavut.Filter
import Nunavut.Newtypes
import Nunavut.Util.Dimensions

{--------------------------------------------------------------------------
-                         Types and Constructors                         -
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
instance HasInput Layer where
  inSize = weights . inSize

instance HasOutput Layer where
  outSize = weights . inSize
