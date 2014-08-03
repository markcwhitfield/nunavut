module Nunavut.Layer.WeightsSpec where

import Control.Monad.Trans.RWS (runRWST)
import Data.Either (isLeft, isRight)
import Test.Hspec
import Test.QuickCheck

import Nunavut.Layer.Weights
import Nunavut.Propogation
import Nunavut.Util.Arbitrary
import Nunavut.Util.TestUtils

spec :: Spec
spec = do
  let doBackprop (MatchVM (MatchVV datum e) w) conf =
        let pData = PData [datum] []
            rws = backpropW w e
        in isRight $ runRWST rws conf ([], pData)
  describe "propW" $ do
    isTotal2 (\w sig -> isLeft $ runRWST (propW w sig) () ())
    it "suceeds on dimension match" $ property $
      \(MatchMV w sig) -> isRight $ runRWST (propW w sig) () ()
  describe "backpropW" $ do
    isTotal2 doBackprop
    it "succeeds on dimension match" $ property
      doBackprop
