{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Nunavut.NeuralNet where

import Prelude hiding (concat)

import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Text.Lazy (concat, pack)
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
instance HasInput FFNet where
  inSize = layers . _last . inSize
instance HasOutput FFNet where
  outSize = layers . _head . outSize


{--------------------------------------------------------------------------
-                              Constructors                              -
--------------------------------------------------------------------------}
mkFFNet :: [Layer] -> Either Error FFNet
mkFFNet [] = Left $ mkError "Cannot instantiate empty FFNet"
mkFFNet (l:[]) = Right . FFNet $ l :| []
mkFFNet (l:ls) = mkFFNet ls >>= addLayer l


{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
addLayer :: Layer -> FFNet -> Either Error FFNet
addLayer l n =  if dimsMatch n l
                then Right $ over layers (l <|) n
                else Left $ mkError $ concat [
                           "Dimension Mismatch. New layer input size: ",
                           pack . show $ l ^. inSize,
                           "Current output iize ",
                           pack . show $ n ^. inSize]
