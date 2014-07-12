{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Nunavut.Util.Dimensions where

import Control.Lens (Getter, (^.))

{--------------------------------------------------------------------------
-                                Classes                                 -
--------------------------------------------------------------------------}
class HasOutput a where
  outSize :: Getter a Int

class HasInput a where
  inSize :: Getter a Int

class DimMatchable a b where
  dimsMatch :: a -> b -> Bool

instance (HasOutput a, HasInput b) => DimMatchable a b where
  dimsMatch a b = a ^. outSize == b ^. inSize
