module Nunavut.Propogation where

import Nunavut.Newtypes
import Nunavut.Util

class Propogate a where
  unsafePropogate :: a -> Signal -> Signal
  propogate :: a -> Signal -> Either Error Signal
  propogate a = return . unsafePropogate a
