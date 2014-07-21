{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Nunavut.Layer.Internal where

import Control.Lens (makeLenses, views)
import Data.List (intercalate)

import Nunavut.Activator
import Nunavut.Filter
import Nunavut.Layer.Weights
import Nunavut.Propogation
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

instance SizedOperator Layer where
  inSize = weights . inSize
  outSize = weights . outSize

instance Propogate Layer where
  unsafePropogate l = across l unsafePropogate
  propogate l = across l propogate

{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
across ::
  (Monad m)
  => Layer
  -> (forall a. Propogate a => a -> Signal -> m Signal)
  -> Signal
  -> m Signal
across l f a = prop filterL =<< prop activator =<< prop weights a
  where prop getter = views getter f l
