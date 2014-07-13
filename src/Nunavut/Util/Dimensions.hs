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
class HasOutput a where
  outSize :: Getter a Int

class HasInput a where
  inSize :: Getter a Int

class DimMatchable a b where
  dimsMatch :: a -> b -> Bool

{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance (HasOutput a, HasInput b) => DimMatchable a b where
  dimsMatch a b = a ^. outSize == b ^. inSize

{--------------------------------------------------------------------------
-                            Helper Functions                            -
--------------------------------------------------------------------------}
dimMismatch :: (HasOutput a, HasInput b) => a -> b -> Error
dimMismatch a b = mkError $ concat [
                  "Dimension Mismatch: ",
                  "Output size ", pack . show $ a ^. outSize,
                  "Does not match input size ", pack . show $ b ^. inSize] 

ifDimsMatch :: (HasOutput a, HasInput b) => (a -> b -> c) -> a -> b -> Either Error c
ifDimsMatch f a b = if dimsMatch a b
                    then Right $ f a b
                    else Left $ dimMismatch a b
