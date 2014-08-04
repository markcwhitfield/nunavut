module Nunavut.Layer.WeightsSpec where

import Control.Monad.Trans.RWS (evalRWST)
import Data.Either (isRight)
import Test.Hspec
import Test.QuickCheck

import Nunavut.Layer.Weights
import Nunavut.Propogation
import Nunavut.Util.Arbitrary
import Nunavut.Util.TestUtils

spec :: Spec
spec = do
  let doBackprop w e datum conf =
        let pData = PData [datum] []
            rws = backpropW w e
        in case evalRWST rws conf ([], pData) of
          Right (res, _) -> res `seq` True
          _              -> False
  describe "propW" $ do
    isTotal2 (\w sig -> isRight . fmap fst $ evalRWST (propW w sig) () ())
    it "suceeds on dimension match" $ property $
      \(MatchMV w sig) -> isRight . fmap fst $ evalRWST (propW w sig) () ()
  describe "backpropW" $ do
    isTotal4 doBackprop
    it "succeeds on dimension match" $ property $
      \(MatchVM (MatchVV datum e) w) conf -> 
        doBackprop w e datum conf
