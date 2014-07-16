{-# LANGUAGE TemplateHaskell #-}
module Nunavut.NeuralNet where

import Prelude hiding (concat)

import Control.Monad (liftM)
import Data.Foldable (foldrM)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Control.Lens (makeLenses, over, (^.))

import Nunavut.Layer
import Nunavut.Newtypes
import Nunavut.Util

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
data FFNet = FFNet {
             _layers :: NonEmpty Layer
             }

makeLenses ''FFNet

{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance SizedOperator FFNet where
  inSize = layers . _last . inSize
  outSize = layers . _head . outSize


{--------------------------------------------------------------------------
-                              Constructors                              -
--------------------------------------------------------------------------}
mkFFNet :: [Layer] -> Either Error FFNet
mkFFNet [] = Left $ mkError "Cannot instantiate empty FFNet"
mkFFNet (l:[]) = Right . FFNet $ l :| []
mkFFNet (l:ls) = addLayer l =<< mkFFNet ls


{--------------------------------------------------------------------------
-                              Propogation                               -
--------------------------------------------------------------------------}
propogate :: FFNet -> Input -> Either Error Activations
propogate n i = foldrM foldProp (i':|[]) $ n ^. layers
  where foldProp l as@(a:|_) = liftM (<| as) $ propL l a
        i'                   = mkActiv . unInput $ i
 

{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
addLayer :: Layer -> FFNet -> Either Error FFNet
addLayer = ifDimsMatch doAdd
  where doAdd l = over layers (l <|)
