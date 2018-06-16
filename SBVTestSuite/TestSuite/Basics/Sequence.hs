{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TestSuite.Basics.Sequence
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Test the sequence functions.
-- Most of these tests are adopted from <https://rise4fun.com/z3/tutorialcontent/sequences>
-----------------------------------------------------------------------------

module TestSuite.Basics.Sequence(tests)  where

import Data.SBV
import Data.SBV.Control
import Utils.SBVTestFramework

import           Data.SBV.Sequence ((.++))
import qualified Data.SBV.Sequence as S

import Control.Monad (unless)

-- Test suite
tests :: TestTree
tests =
  testGroup "Basics.Sequene" [
      goldenCapturedIO "strConcatSat"    $ \rf -> checkWith z3{redirectVerbose=Just rf} seqConcatSat    Sat
    , goldenCapturedIO "strConcatUnsat"  $ \rf -> checkWith z3{redirectVerbose=Just rf} seqConcatUnsat  Unsat
    , goldenCapturedIO "strIndexOfSat"   $ \rf -> checkWith z3{redirectVerbose=Just rf} seqIndexOfSat   Sat
    , goldenCapturedIO "strIndexOfUnsat" $ \rf -> checkWith z3{redirectVerbose=Just rf} seqIndexOfUnsat Unsat
    ]

checkWith :: SMTConfig -> Symbolic () -> CheckSatResult -> IO ()
checkWith cfg props csExpected = runSMTWith cfg{verbose=True} $ do
        _ <- props
        query $ do cs <- checkSat
                   unless (cs == csExpected) $
                     case cs of
                       Unsat -> error "Failed! Expected Sat, got UNSAT"
                       Sat   -> getModel         >>= \r -> error $ "Failed! Expected Unsat, got SAT:\n" ++ show (SatResult (Satisfiable cfg r))
                       Unk   -> getUnknownReason >>= \r -> error $ "Failed! Expected Unsat, got UNK:\n" ++ show r

seqConcatSat :: Symbolic ()
seqConcatSat = constrain $
      literalList @Integer [1,2,3] .++ literalList [4,5,6]
  .== literalList @Integer [1..6]

seqConcatUnsat :: Symbolic ()
seqConcatUnsat = constrain $
      literalList @Integer [1,2,3] .++ literalList [4,5,6]
  .== literalList @Integer [1..7]

seqIndexOfSat :: Symbolic ()
seqIndexOfSat = constrain $
      S.indexOf (literalList @Integer [1,2,3,1,2,3]) (literalList [1])
  .== (0 :: SInteger)

seqIndexOfUnsat :: Symbolic ()
seqIndexOfUnsat = constrain $
      S.indexOf (literalList @Integer [1,2,3,1,2,3]) (literalList [1])
  ./= (0 :: SInteger)
