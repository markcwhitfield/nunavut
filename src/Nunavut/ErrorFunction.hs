module Nunavut.ErrorFunction (
  ErrorFunction,
  errType,
  getErr,
  getErrSig,
  sumOfSquares
  ) where

import Control.Lens (Lens', lens)
import Numeric.LinearAlgebra (cmap, sumElements)

import Nunavut.Newtypes (fromVec, toVec, Label)
import Nunavut.Signals (Signal, ErrorSignal)

data ErrFType = SumOfSquares
  deriving (Eq, Show)

data ErrorFunction = ErrF { 
  _errType      :: ErrFType,
  _getErrSig :: Label -> Signal -> ErrorSignal,
  _getErr    :: Label -> Signal -> Double
  }
instance Show ErrorFunction where
  show (ErrF errT _ _) = show errT

errType :: Lens' ErrorFunction ErrFType
errType = lens _errType (\ef et -> ef { _errType = et })

getErrSig :: Lens' ErrorFunction (Label -> Signal -> ErrorSignal)
getErrSig = lens _getErrSig (\ef et -> ef { _getErrSig = et })

getErr :: Lens' ErrorFunction (Label -> Signal -> Double)
getErr = lens _getErr (\ef et -> ef { _getErr = et })

sumOfSquares :: ErrorFunction
sumOfSquares = ErrF SumOfSquares sosErr sos
  where sosErr lbl sig = fromVec $ - (toVec lbl - toVec sig)
        sos lbl sig = (* 0.5) . sumElements . cmap (** 2) $ lbl' - sig'
          where lbl'        = toVec lbl
                sig'        = toVec sig
