-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Examples.Misc.UnsatCore
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Demonstrates extraction of unsat-cores.
-----------------------------------------------------------------------------

module Data.SBV.Examples.Misc.UnsatCore where

import Data.SBV

-- | A simple program with a few constraints
p :: Goal
p = do a <- sInteger "a"
       b <- sInteger "b"

       -- create constraints, but give them names
       namedConstraint "less than 5"  $ a .<= 5
       namedConstraint "more than 10" $ a .>= (10::SInteger)
       namedConstraint "irrelevant"   $ a .>= b

       -- we can also label arbitrary internal terms, if necessary
       constrain $ (a + b) .<= label "really not relevant" (a + b + 12)

-- | Extract the unsat-core of 'p'. We have:
--
-- >>> ucCore
-- Unsatisfiable. Unsat core:
--   less than 5
--   more than 10
-- =====================================
-- Unsat core is: ["less than 5","more than 10"]
--
-- Demonstrating the use of unsat-cores.
ucCore :: IO ()
ucCore = do r <- satWith z3{getUnsatCore=True} p
            print r
            putStrLn "====================================="
            case extractUnsatCore r of
              Nothing -> putStrLn "No unsat core!"
              Just xs -> putStrLn $ "Unsat core is: " ++ show xs
