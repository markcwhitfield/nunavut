module Nunavut.NeuralNetSpec where

import Control.Lens ((^.))
import Control.Monad.Trans.RWS (runRWST)
import Data.Either (isRight)
import Debug.Trace (traceShow)
import Test.QuickCheck
import Test.Hspec

import Nunavut.NeuralNet
import Nunavut.Propogation
import Nunavut.Util.Arbitrary
import Nunavut.Util.TestUtils

spec :: Spec
spec = do
  let doProp net sig = isRight $ runRWST (propogate net sig) () ()
  let doBackprop net err pdata config = 
        not (null (pdata ^. preWeights) || null (pdata ^. preActivated)) ==>
        isRight $ runRWST (backprop net err) config ([], pdata)
  describe "propogate" $ do
    isTotal2 doProp 
    it "always succeeds on dimension match" $ property $
      \(MatchMV net sig) -> 
        {-traceShow net $ traceShow (net ^. inSize) $ traceShow (net ^. outSize) $-}
        {-net ^. inSize > 1 && net ^. outSize > 1 ==>-}
        doProp net sig

  describe "backprop" $ do
    isTotal4 doBackprop
    it "always succeeds on dimension match" $ property $
      \(MatchVT err (MatchTT net pdata)) config -> doBackprop net err pdata config

  describe "updateWeights" $ do
    isTotal2 (\net upd -> isRight $ updateWeights net upd)
    it "always suceeds on dimension match" $ property $
      \(MatchTT net upd) -> isRight $ updateWeights net upd

  describe "train" $ do
    isTotal3 (\conf net inps -> isRight $ train conf net inps)
    it "always suceeds on dimension match" $ property $
      \(MatchVM lbl (MatchMV net inp)) conf -> isRight $ train conf net [(inp,lbl)]
