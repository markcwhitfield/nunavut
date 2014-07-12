{-# LANGUAGE TemplateHaskell #-}
module Nunavut.Activator (
  Activator(..),
  runActivator,
  runADeriv
  ) where

import Control.Lens (makeLenses)

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
data Activator = Activator {
                 _runActivator :: Double -> Double,
                 _runADeriv    :: Double -> Double
                 }
makeLenses ''Activator
