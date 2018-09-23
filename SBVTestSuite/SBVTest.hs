-----------------------------------------------------------------------------
-- |
-- Module      :  SBVTestSuite.SBVTest.Main
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Main entry point to the test suite
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Main(main) where

import Test.Tasty

import Utils.SBVTestFramework (getTestEnvironment, TestEnvironment(..), CIOS(..), pickTests)

import System.Exit (exitSuccess)

import qualified TestSuite.Basics.List

-- On remote machines for Appveyor/Travis, the build machines doesn't have enough memory
-- and/or powerful enough to run our heavy tests; so we skip tests for Windows hosts and
-- reduce them for OSX. For Linux, we run them all. Note that this is only for remote
-- hosts; when we run locally, all tests are run.
--
-- TODO: Would be nice to run them all on Windows/OSX on remote hosts as well.
ciFilter :: CIOS -> Int -> TestTree -> IO TestTree
ciFilter _  100 tt = return tt
ciFilter os   n tt = do putStrLn $ "OS: " ++ show os ++ ", Running only " ++ show n ++ "% of arithmetic tests."
                        pickTests n tt

main :: IO ()
main = do (testEnv, _testPercentage) <- getTestEnvironment

          putStrLn $ "SBVTest: Test platform: " ++ show testEnv

          defaultMain $ testGroup "Local" [TestSuite.Basics.List.tests]
