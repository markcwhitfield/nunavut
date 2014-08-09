module Nunavut.ErrorFunction (
  ErrorFunction,
  errType,
  getErr,
  getErrSig,
  sumOfSquares
  ) where

import           Control.Lens          (Lens', lens)
import           Numeric.LinearAlgebra (cmap, sumElements)

import           Nunavut.Newtypes      (Label, fromVec, toVec)
import           Nunavut.Signals       (ErrorSignal, Signal)

{--------------------------------------------------------------------------
-                                 Types                                  -
--------------------------------------------------------------------------}
data ErrFType = SumOfSquares
  deriving (Eq, Show)
data ErrorFunction = ErrF {
  _errType   :: ErrFType,
  _getErrSig :: Label -> Signal -> ErrorSignal,
  _getErr    :: Label -> Signal -> Double
  }

type MkErrSig = Label -> Signal -> ErrorSignal
type MkErr    = Label -> Signal -> Double


{--------------------------------------------------------------------------
-                               Instances                                -
--------------------------------------------------------------------------}
instance Show ErrorFunction where
  show (ErrF errT _ _)            = show errT

{--------------------------------------------------------------------------
-                             Error Functons                             -
--------------------------------------------------------------------------}
sumOfSquares :: ErrorFunction
sumOfSquares                              = ErrF SumOfSquares sosErr sos
  where sosErr lbl sig                    = fromVec $ - (toVec lbl - toVec sig)
        sos lbl sig                       = (* 0.5) . sumElements . cmap (** 2) $ lbl' - sig'
          where lbl'                      = toVec lbl
                sig'                      = toVec sig

{--------------------------------------------------------------------------
-                                 Lenses                                 -
--------------------------------------------------------------------------}
errType :: Lens' ErrorFunction ErrFType
errType                                   = lens _errType (\ef et -> ef { _errType = et })

getErrSig :: Lens' ErrorFunction MkErrSig
getErrSig                                 = lens _getErrSig (\ef et -> ef { _getErrSig = et })

getErr :: Lens' ErrorFunction MkErr
getErr                                    = lens _getErr (\ef et -> ef { _getErr = et })
