module Nunavut.ActivatorSpec where

import Control.Lens ((^.))
import Test.Hspec
import Test.QuickCheck

import Nunavut.Activator
import Nunavut.Util.Arbitrary ()
import Nunavut.Util.TestUtils

spec :: Spec
spec = do
  describe "Activator" $ do
    describe "activatorFunc" $ do
      isTotal2 (\a z -> (a ^. activatorFunc) z)
      it "is always monotonically increasing" $ property $
        \a z z' -> z > z' ==>
          (a ^. activatorFunc) z >= (a ^. activatorFunc) z'

    describe "activatorDeriv" $ do
      isTotal2 (\a z -> (a ^. activatorDeriv) z)
      it "is the derivative of the activatorFunc" $ property $
        \a z -> abs z > eps ==> diff (a ^. activatorFunc) z - (a ^. activatorDeriv) z < eps

  describe "logisticFunc" $ do
    let logisticFunc = logistic ^. activatorFunc
    it "has a codomain of [0,1]" $ property $
      \z -> logisticFunc z >= 0 && logisticFunc z <= 1
  describe "logisticDeriv" $ do
    let logisticDeriv = logistic ^. activatorDeriv
    it "has a codomain of [0,0.25]" $ property $
      \z -> logisticDeriv z >= 0 && logisticDeriv z <= 0.25

  describe "reluFunc" $ do
    let reluFunc = relu ^. activatorFunc
    it "has a codomain of [0,inf)" $ property $
      \z -> reluFunc z >= 0
  describe "reluDeriv" $ do
    let reluDeriv = relu ^. activatorDeriv
    it "has a codomain of {0,1}" $ property $
      \z -> reluDeriv z == 0 || reluDeriv z == 1

  describe "tanhFunc" $ do
    let tanhFunc = tanhActivator ^. activatorFunc
    it "has a codomain of [-1,1]" $ property $
      \z -> tanhFunc z >= -1 && tanhFunc z <= 1
  describe "tanhDeriv" $ do
    let tanhDeriv = tanhActivator ^. activatorDeriv
    it "has a codomain of [0,1]" $ property $
      \z -> tanhDeriv z >= 0 && tanhDeriv z <= 1
