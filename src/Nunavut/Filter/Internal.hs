{-# LANGUAGE TemplateHaskell #-}
module Nunavut.Filter.Internal where

import Control.Lens (makeLenses, (^.))

import Nunavut.Newtypes
import Nunavut.Propogation

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
data FilterType = None | Softmax
  deriving (Show, Eq)
data Filter = Filter {
              _filterType :: FilterType,
              _filterFunc :: Signal -> Signal,
              _filterDeriv :: ErrorSignal -> Jacobian
              }
makeLenses ''Filter

{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance Show Filter where
  show = show . (^. filterType)
instance Propogate Filter where
  unsafePropogate n = n ^. filterFunc
