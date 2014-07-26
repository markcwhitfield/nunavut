{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Nunavut.Layer.Internal where

import Control.Lens (makeLenses, to, views)
import Data.List (intercalate)

import Nunavut.Activator
import Nunavut.Layer.Weights
import Nunavut.Util

{--------------------------------------------------------------------------
-                         Types and Constructors                         -
--------------------------------------------------------------------------}
data Layer = Layer {
             _weights   :: Weights,
             _activator :: Activator
             }
makeLenses ''Layer

{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance Show Layer where
  show (Layer w a) = concat [
                       "Layer:\n",
                       "\tWeights:\n\t\t",
                       intercalate "\n\t\t" . lines . show $ w,
                       "\n\n\tActivator:\n\t\t",
                       show a]

instance SizedOperator Layer where
  inSize = weights . inSize
  outSize = to $ views (weights . outSize) (+ 1)
