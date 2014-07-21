{-# LANGUAGE TemplateHaskell #-}
module Nunavut.Activator.Internal where

import Control.Lens (makeLenses, (^.))
import Control.Monad.Writer (tell)
import Data.Monoid (mempty)

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
  unsafePropogate a sig = do
    tell $ PData mempty [sig] mempty
    return $ elementwise (a ^. activatorFunc) sig

  unsafeBackprop a sig = return . elementwise (a ^. activatorDeriv) $ sig
