module Nunavut.ActivatorSpec where

import Control.Lens ((^.))
import Control.Monad.Writer (runWriter)
import Control.Monad.Trans.RWS (evalRWST)
import Data.Either (isRight)
import Numeric.LinearAlgebra (flatten, sub, minElement, maxElement, prodElements)
import Test.Hspec
import Test.QuickCheck

import Nunavut.Activator
import Nunavut.Newtypes
import Nunavut.Propogation
import Nunavut.Util.Arbitrary
import Nunavut.Util.TestUtils

spec :: Spec
spec = do
  describe "propA" $
    isTotal2 (\a s -> fst . runWriter $ propA a s)

  describe "backpropA" $
    isTotal3 (\a e datum conf ->
      isRight . fmap fst $ evalRWST (backpropA a e) conf ([], PData [] [datum]))

  describe "activatorFunc" $ do
    isTotal2 (\a z -> (a ^. activatorFunc) z)

    describe "logisticFunc" $ do
      let logisticFunc = logistic ^. activatorFunc
      it "has a codomain of [0,1]" $ property $
        \z -> logisticFunc z >= addConst (-eps) 0 && logisticFunc z <= addConst eps 1

    describe "reluFunc" $ do
      let reluFunc = relu ^. activatorFunc
      it "has a codomain of [0,inf)" $ property $
        \z -> reluFunc z >= addConst (-eps) 0

    describe "tanhFunc" $ do
      let tanhFunc = tanhActivator ^. activatorFunc
      it "has a codomain of [-1,1]" $ property $
        \z -> tanhFunc z >= addConst (-eps) (-1) && tanhFunc z <= addConst eps 1

  describe "activatorDeriv" $ do
    isTotal2 (\a z -> (a ^. activatorDeriv) z)
    it "is the derivative of the activatorFunc" $ property $
      \f (SmallVec v) ->
        let func = (f ^. activatorFunc)
            j = toMtx . (f ^. activatorDeriv) . fromVec . toVec $ v
            numericalJ = toMtx $ diffJacob func v
        in  prodElements (toVec v) /= 0 ==> -- reLU nondifferentiable along axes
              (l2Norm . mkInput . flatten $ j `sub` numericalJ) < eps

    describe "logisticDeriv" $ do
      let logisticDeriv = logistic ^. activatorDeriv
      it "has a codomain of [0,0.25]" $ property $
        \z -> (\x -> minElement x >= -eps && maxElement x <= 0.25 + eps) . toMtx . logisticDeriv $ z

    describe "reluDeriv" $ do
      let reluDeriv = relu ^. activatorDeriv
      it "has a codomain of {0,1}" $ property $
        \z -> (\x -> minElement x `elem` [0,1] && maxElement x `elem` [0,1]) . toMtx . reluDeriv $ z

    describe "tanhDeriv" $ do
      let tanhDeriv = tanhActivator ^. activatorDeriv
      it "has a codomain of [0,1]" $ property $
        \z -> (\x -> minElement x >= 0 && maxElement x <= 1) . toMtx . tanhDeriv $ z
