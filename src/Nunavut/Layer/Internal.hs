{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Nunavut.Layer.Internal where

import Control.Lens (makeLenses, to, views)

import Nunavut.Activator (Activator)
import Nunavut.Layer.Weights (Weights, shape)
import Nunavut.Util (SizedOperator(..))

{--------------------------------------------------------------------------
-                         Types and Constructors                         -
--------------------------------------------------------------------------}
data Layer = Layer {
             _activator :: Activator,
             _weights   :: Weights
             }
makeLenses ''Layer

{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance Show Layer where
  show (Layer a w) = concat [
                       "Layer:",
                       "\n\tWeights:\n\t\t",
                       show (shape w),
                       "\n\tActivator:\n\t\t",
                       show a]

instance SizedOperator Layer where
  inSize = weights . inSize
  outSize = to $ views (weights . outSize) succ -- + bias element
