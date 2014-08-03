module Nunavut.LayerSpec where

import Control.Monad.Trans.RWS (runRWST)
import Data.Either (isLeft, isRight)
import Test.Hspec
import Test.QuickCheck

import Nunavut.Layer
import Nunavut.Propogation
import Nunavut.Util.Arbitrary
import Nunavut.Util.TestUtils

spec :: Spec
spec = do
  let doBackprop (MatchVM (MatchVV datum e) (MatchMV l datum')) conf =
        let pData = PData [datum'] [datum]
            rws = backpropL l e
        in isRight $ runRWST rws conf ([], pData)
  describe "propL" $ do
    isTotal2 (\l sig -> isLeft $ runRWST (propL l sig) () ())
    it "suceeds on dimension match" $ property $
      \(MatchMV l sig) -> isRight $ runRWST (propL l sig) () ()
  describe "backpropL" $ do
    isTotal2 doBackprop
    it "succeeds on dimension match" $ property
      doBackprop
