{-# LANGUAGE TemplateHaskell #-}
module Nunavut.Layer(
  Layer(..),
  weights,
  activator,
  filterL,
  inSize,
  outSize
  ) where

import Control.Lens (makeLenses, (^.))
import Data.List (intercalate)

import Nunavut.Activator
import Nunavut.Filter
import Nunavut.Newtypes
import Nunavut.Util

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
instance Show Layer where
  show (Layer w a f) = concat [
                       "Layer:\n",
                       "\tWeights:\n\t\t",
                       intercalate "\n\t\t" . lines . show $ w,
                       "\n\n\tActivator:\n\t\t",
                       show a,
                       "\n\n\tFilter:\n\t\t",
                       show f]


instance HasInput Layer where
  inSize = weights . inSize

instance HasOutput Layer where
  outSize = weights . inSize
