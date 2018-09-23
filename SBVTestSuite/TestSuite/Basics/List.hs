-----------------------------------------------------------------------------
-- |
-- Module      :  TestSuite.Basics.List
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Test the sequence/list functions.
-- Most of these tests are adopted from <http://rise4fun.com/z3/tutorialcontent/sequences>
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestSuite.Basics.List(tests)  where

import Data.SBV.Control
import Utils.SBVTestFramework

import Data.SBV.List ((.!!), (.++), (.:))
import qualified Data.SBV.List as L

import Control.Monad (unless)


-- Test suite
tests :: TestTree
tests =
  testGroup "Basics.List" [
      -- goldenCapturedIO "seqConcatGood" $ \rf -> checkWith z3{redirectVerbose=Just rf} seqConcatSat    Sat
    -- , goldenCapturedIO "seqConcatBad"  $ \rf -> checkWith z3{redirectVerbose=Just rf} seqConcatUnsat  Unsat
      testCase "seqExamples2"  $ assertIsntSat seqExamples2
    -- , goldenCapturedIO "seqExamples4"  $ \_ -> checkWith z3{verbose=True} seqExamples3    Sat
    , testCase "seqExamples3"  $ assertIsntSat seqExamples3
    , testCase "seqExamples4"  $ assertIsntSat seqExamples4
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

-- seqConcatSat :: Symbolic ()
-- seqConcatSat = constrain $ [1..3] .++ [4..6] .== ([1..6] :: SList Integer)

-- seqConcatUnsat :: Symbolic ()
-- seqConcatUnsat = constrain $ [1..3] .++ [4..6] .== ([1..7] :: SList Integer)

seqExamples2 :: Symbolic ()
seqExamples2 = do
  [a, b] <- sIntegers ["a", "b"]
  let lst :: SList Integer
      lst = L.implode [a, b]

  constrain $ L.length lst ./= 2

seqExamples3 :: Symbolic ()
seqExamples3 = do
  [a, b] <- sIntegers ["a", "b"]
  let lst :: SList Integer
      lst = L.implode [a, b]
  constrain $ a .> 0
  constrain $ b .>= 0
  constrain $ L.sum lst .== 0

seqExamples4 :: Symbolic ()
seqExamples4 = do
  lst <- (sList "lst" :: Symbolic (SList Integer))
  constrain $ L.head lst .> 0
  constrain $ L.head (L.tail lst) .>= 0
  constrain $ L.length lst .== 2
  constrain $ L.sum lst .== 0
