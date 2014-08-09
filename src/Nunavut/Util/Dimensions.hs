{-# LANGUAGE MultiParamTypeClasses #-}
module Nunavut.Util.Dimensions where

import Prelude hiding (concat)

import Control.Lens (Getter, (^.))
import Data.Text.Lazy (pack, concat, Text)

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
checkDims' :: (SizedOperator a, SizedOperator b) => Text -> a -> b -> Either Error b
checkDims' name = ifDimsMatch name (\_ b -> b)

checkDims :: (SizedOperator a, SizedOperator b) => Text -> a -> b -> Either Error a
checkDims name = ifDimsMatch name const

dimMismatch :: (SizedOperator a, SizedOperator b) => Text -> a -> b -> Error
dimMismatch name a b = mkError . concat $ [
                  name, ": ",
                  "Dimension Mismatch: ",
                  "Output size ", pack . show $ a ^. outSize,
                  "Does not match input size ", pack . show $ b ^. inSize] 

ifDimsMatch :: (SizedOperator a, SizedOperator b) => Text -> (a -> b -> c) -> a -> b -> Either Error c
ifDimsMatch name f a b = if dimsMatch a b
                         then Right $ f a b
                         else Left $ dimMismatch name a b
