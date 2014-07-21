{-# LANGUAGE TemplateHaskell #-}
module Nunavut.Filter.Internal where

import Control.Lens (makeLenses, (^.))
import Control.Monad.Reader (ask)
import Control.Monad.Writer (tell)
import Data.Monoid (mempty)

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
              _filterDeriv :: Signal -> Jacobian
              }
makeLenses ''Filter

{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance Show Filter where
  show = show . (^. filterType)
instance Propogate Filter where
  unsafePropogate f sig = do
    tell $ PData mempty mempty [sig]
    return $ (f ^. filterFunc) sig

  unsafeBackprop f err = do
    datum <- ask
    let jac = (f ^. filterDeriv) (datum ^. dPreFiltered)
    return $ trans jac <> err
