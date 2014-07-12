{-# LANGUAGE TemplateHaskell #-}
module Nunavut.Filter (
  Filter(..),
  runFilter,
  runFDeriv
  ) where

import Control.Lens (makeLenses)

import Nunavut.Newtypes

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
data Filter = Filter {
              _runFilter :: Activation -> Activation,
              _runFDeriv :: Activation -> Activation
              }
makeLenses ''Filter
