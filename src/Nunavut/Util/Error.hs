module Nunavut.Util.Error(
  Error,
  mkError,
  unError
  ) where

import           Data.Text.Lazy (Text)

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
newtype Error = Error { unError :: Text }
  deriving (Show)

mkError :: Text -> Error
mkError                  = Error
