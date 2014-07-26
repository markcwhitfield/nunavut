module Nunavut.NewtypesSpec where

import Test.Hspec

import Nunavut.Newtypes
import Nunavut.Newtypes.Laws
import Nunavut.Util.Arbitrary ()

spec :: Spec
spec = do
  describe "Input" $
    validHasVec (undefined :: Input)

  describe "Label" $
    validHasVec (undefined :: Label)

  describe "Jacobian" $
    validHasMtx (undefined :: Jacobian)
