{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Nunavut.Util.Dimensions where

import Prelude hiding (concat)

import Control.Lens (Getter, (^.))
import Data.Text.Lazy (pack, concat)

import Nunavut.Util.Error

{--------------------------------------------------------------------------
-                                Classes                                 -
--------------------------------------------------------------------------}
class SizedOperator a where
  inSize    :: Getter a Int
  outSize   :: Getter a Int
  dimsMatch :: SizedOperator b => a -> b -> Bool
  dimsMatch a b = a ^. outSize == b ^. inSize

{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
dimMismatch :: (SizedOperator a, SizedOperator b) => a -> b -> Error
dimMismatch a b = mkError $ concat [
                  "Dimension Mismatch: ",
                  "Output size ", pack . show $ a ^. outSize,
                  "Does not match input size ", pack . show $ b ^. inSize] 

ifDimsMatch :: (SizedOperator a, SizedOperator b) => (a -> b -> c) -> a -> b -> Either Error c
ifDimsMatch f a b = if dimsMatch b a
                    then Right $ f a b
                    else Left $ dimMismatch b a
