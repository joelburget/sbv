{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Sequence
-- Copyright   :  (c) Joel Burget, Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- A collection of utilities for working with symbolic sequences. To the extent
-- possible, the functions in this module follow those of "Data.List" so
-- importing qualified is the recommended workflow.
-----------------------------------------------------------------------------

module Data.SBV.Sequence (
  -- * Length, emptiness
    length, null
  -- * Deconstructing/Reconstructing
  , head, tail, elemToSeq, seqToSeqAt, elemAt, (.!!), implode, concat, (.++)
  -- * Containment
  , isInfixOf, isSuffixOf, isPrefixOf
  -- * Subsequences
  , take, drop, subSeq, replace, indexOf, offsetIndexOf
  ) where

import Prelude hiding (head, tail, length, take, drop, concat, null)
import qualified Prelude as P

import Data.SBV.Core.Data
import Data.SBV.Core.Model

import Data.List (genericLength, genericIndex, genericDrop, genericTake)
import qualified Data.List as L (tails, isSuffixOf, isPrefixOf, isInfixOf)

-- For doctest use only
--
-- $setup
-- >>> import Data.SBV.Provers.Prover (prove, sat)
-- >>> import Data.SBV.Utils.Boolean  ((==>), (&&&), bnot, (<=>))

-- | Short cut for `concat`.
--
-- >>> sat $ \x y z -> S.length x .== 2 &&& S.length y .== 1 &&& x .++ y .++ z .== literal ['a'..'e']
-- Satisfiable. Model:
--   s0 = [1, 2] :: [Char]
--   s1 =    [3] :: [Char]
--   s2 = [4, 5] :: [Char]
infixr 5 .++
(.++) :: SymWord a => SBV [a] -> SBV [a] -> SBV [a]
(.++) = concat

-- | Short cut for 'elemAt'
(.!!) :: SymWord a => SBV [a] -> SInteger -> SBV a
(.!!) = elemAt

-- | Length of a sequence.
--
-- >>> sat $ \s -> length s .== 2
-- >>> sat $ \s -> length s .< 0
length :: SymWord a => SSequence a -> SInteger
length = lift1l (SeqOp SeqLen) (Just (fromIntegral . P.length))

-- | @`null` s@ is True iff the sequence is empty
--
-- >>> prove $ \s -> null s <=> length s .== 0
-- >>> prove $ \s -> null s <=> s .== (literal [] :: SSequence Integer)
null :: SymWord a => SSequence a -> SBool
null s
  | Just cs <- unliteralList s
  = literal (P.null cs)
  | True
  = s .== literalList []

-- | @`head`@ returns the head of a sequence. Unspecified if the sequence is
-- empty.
--
-- >>> prove $ \c -> head (elemToSeq c) .== c
head :: SymWord a => SSequence a -> SBV a
head = (`elemAt` 0)

-- | @`tail`@ returns the tail of a sequence. Unspecified if the sequence is
-- empty.
--
-- >>> prove $ \h s -> tail (elemToSeq h .++ s) .== s
-- Q.E.D.
-- >>> prove $ \s -> length s .> 0 ==> length (tail s) .== length s - 1
-- Q.E.D.
-- >>> prove $ \s -> bnot (null s) ==> elemToSeq (head s) .++ tail s .== s
-- Q.E.D.
tail :: SymWord a => SSequence a -> SSequence a
tail s
 | Just (_:cs) <- unliteralList s
 = literalList cs
 | True
 = subSeq s 1 (length s - 1)

-- | @`elemToSeq` c@ is the sequence of length 1 that contains only the
-- value @c@.
--
-- >>> prove $ \c -> c .== literal 'A' ==> elemToSeq c .== literal ['A']
-- Q.E.D.
-- >>> prove $ \c -> length (elemToSeq c) .== 1
-- Q.E.D.
elemToSeq :: SymWord a => SBV a -> SSequence a
elemToSeq = lift1r (SeqOp SeqUnit) (Just wrap)
  where wrap c = [c]

-- | @`seqToSeqAt` s offset@. Subsequence of length 1 at @offset@ in @s@.
-- Unspecified if index is out of bounds.
--
-- >>> prove $ \s1 s2 -> seqToSeqAt (s1 .++ s2) (length s1) .== seqToSeqAt s2 0
-- Q.E.D.
-- >>> sat $ \s -> S.length (s :: SSequence Integer) .>= 2 &&& S.seqToSeqAt s 0 ./= S.seqToSeqAt s (S.length s - 1)
-- Satisfiable. Model:
--   s0 = [8, 12, 14] :: [Integer]
seqToSeqAt :: SymWord a => SSequence a -> SInteger -> SSequence a
seqToSeqAt s offset = subSeq s offset 1

-- | @`elemAt` s i@ is the value stored at location @i@. Unspecified if index
-- is out of bounds.
--
-- >>> prove $ \i -> i .>= 0 &&& i .<= 4 ==> literal ['A', 'A', 'A', 'A', 'A'] `elemAt` i .== literal 'A'
-- Q.E.D.
-- >>> prove $ \s i c -> s `elemAt` i .== c ==> indexOf s (elemToSeq c) .<= i
-- Q.E.D.
elemAt :: forall a. SymWord a => SSequence a -> SInteger -> SBV a
elemAt s i
  | Just cs <- unliteralList s, Just ci <- unliteral i, ci >= 0, ci < genericLength cs, let c = cs `genericIndex` ci
  = literal c
  | True
  = SBV (SVal kElem (Right (cache (y (s `seqToSeqAt` i)))))
  where kElem = kindOf (undefined :: a)
        kSeq  = KSequence kElem
        -- This is trickier than it needs to be, but necessary since there's
        -- no SMTLib function to extract the element from a sequence. Instead,
        -- we form a singleton sequence, and assert that it is equivalent to
        -- the extracted value. See <http://github.com/Z3Prover/z3/issues/1302>
        y si st = do c <- internalVariable st kElem
                     cs <- newExpr st kSeq (SBVApp (SeqOp SeqUnit) [c])
                     let csSBV = SBV (SVal kSeq (Right (cache (\_ -> return cs))))
                     internalConstraint st [] $ unSBV $ csSBV .== si
                     return c

-- | @`implode` cs@ is the sequence of length @|cs|@ containing precisely those
-- elements. Note that there is no corresponding function @explode@, since we
-- wouldn't know the length of a symbolic sequence.
--
-- >>> prove $ \c1 c2 c3 -> length (implode [c1, c2, c3]) .== 3
-- Q.E.D.
-- >>> prove $ \c1 c2 c3 -> map (strToCharAt (implode [c1, c2, c3])) (map literal [0 .. 2]) .== [c1, c2, c3]
-- Q.E.D.
implode :: SymWord a => [SBV a] -> SSequence a
implode = foldr ((.++) . elemToSeq) (literalList [])

-- | Concatenate two sequences. See also `.++`.
concat :: SymWord a => SSequence a -> SSequence a -> SSequence a
concat x y | isConcretelyEmpty x = y
           | isConcretelyEmpty y = x
           | True                = lift2 (SeqOp SeqConcat) (Just (++)) x y

-- | @`isInfixOf` sub s@. Does @s@ contain the subsequence @sub@?
--
-- >>> prove $ \s1 s2 s3 -> s2 `isInfixOf` (s1 .++ s2 .++ s3)
-- Q.E.D.
-- >>> prove $ \s1 s2 -> s1 `isInfixOf` s2 &&& s2 `isInfixOf` s1 <=> s1 .== s2
-- Q.E.D.
isInfixOf :: SymWord a => SSequence a -> SSequence a -> SBool
sub `isInfixOf` s
  | isConcretelyEmpty sub
  = literal True
  | True
  = lift2 (SeqOp SeqContains) (Just (flip L.isInfixOf)) s sub -- NB. flip, since `SeqContains` takes args in rev order!

-- | @`isPrefixOf` pre s@. Is @pre@ a prefix of @s@?
--
-- >>> prove $ \s1 s2 -> s1 `isPrefixOf` (s1 .++ s2)
-- Q.E.D.
-- >>> prove $ \s1 s2 -> s1 `isPrefixOf` s2 ==> subSeq s2 0 (length s1) .== s1
-- Q.E.D.
isPrefixOf :: SymWord a => SSequence a -> SSequence a -> SBool
pre `isPrefixOf` s
  | isConcretelyEmpty pre
  = literal True
  | True
  = lift2 (SeqOp SeqPrefixOf) (Just L.isPrefixOf) pre s

-- | @`isSuffixOf` suf s@. Is @suf@ a suffix of @s@?
--
-- >>> prove $ \s1 s2 -> s2 `isSuffixOf` (s1 .++ s2)
-- Q.E.D.
-- >>> prove $ \s1 s2 -> s1 `isSuffixOf` s2 ==> subSeq s2 (length s2 - length s1) (length s1) .== s1
-- Q.E.D.
isSuffixOf :: SymWord a => SSequence a -> SSequence a -> SBool
suf `isSuffixOf` s
  | isConcretelyEmpty suf
  = literal True
  | True
  = lift2 (SeqOp SeqSuffixOf) (Just L.isSuffixOf) suf s

-- | @`take` len s@. Corresponds to Haskell's `take` on symbolic-sequences.
--
-- >>> prove $ \s i -> i .>= 0 ==> length (take i s) .<= i
-- Q.E.D.
take :: SymWord a => SInteger -> SSequence a -> SSequence a
take i s = iteL (i .<= 0)        (literalList [])
         $ iteL (i .>= length s) s
         $ subSeq s 0 i

-- | @`drop` len s@. Corresponds to Haskell's `drop` on symbolic-sequences.
--
-- >>> prove $ \s i -> length (drop i s) .<= length s
-- Q.E.D.
-- >>> prove $ \s i -> take i s .++ drop i s .== s
-- Q.E.D.
drop :: SymWord a => SInteger -> SSequence a -> SSequence a
drop i s = iteL (i .>= ls) (literalList [])
         $ iteL (i .<= 0)  s
         $ subSeq s i (ls - i)
  where ls = length s

-- | @`subSeq` s offset len@ is the subsequence of @s@ at offset `offset` with length `len`.
-- This function is under-specified when the offset is outside the range of positions in @s@ or @len@
-- is negative or @offset+len@ exceeds the length of @s@. For a friendlier version of this function
-- that acts like Haskell's `take`\/`drop`, see `take`\/`drop`.
--
-- >>> prove $ \s i -> i .>= 0 &&& i .< length s ==> subSeq s 0 i .++ subSeq s i (length s - i) .== s
-- Q.E.D.
-- >>> sat  $ \i j -> subSeq (literal ['h', 'e', 'l', 'l', 'o']) i j .== literal ['e', 'l', 'l']
-- Satisfiable. Model:
--   s0 = 1 :: Integer
--   s1 = 3 :: Integer
-- >>> sat  $ \i j -> subSeq (literal ['h', 'e', 'l', 'l']) i j .== literal ['n', 'o']
-- Unsatisfiable
subSeq :: SymWord a => SSequence a -> SInteger -> SInteger -> SSequence a
subSeq s offset len
  | Just c <- unliteralList s                    -- a constant sequence
  , Just o <- unliteral offset               -- a constant offset
  , Just l <- unliteral len                  -- a constant length
  , let lc = genericLength c                 -- length of the sequence
  , let valid x = x >= 0 && x <= lc          -- predicate that checks valid point
  , valid o                                  -- offset is valid
  , l >= 0                                   -- length is not-negative
  , valid $ o + l                            -- we don't overrun
  = literalList $ genericTake l $ genericDrop o c
  | True                                     -- either symbolic, or something is out-of-bounds
  = lift3 (SeqOp SeqExtract) Nothing s offset len

-- | @`replace` s src dst@. Replace the first occurrence of @src@ by @dst@ in @s@
--
-- >>> prove $ \s -> replace (literal ['a', 'b', 'c']) s (literal ['d', 'e', 'f']) .== literal ['d', 'e', 'f'] ==> s .== literal ['a', 'b', 'c']
-- Q.E.D.
-- >>> prove $ \s1 s2 s3 -> length s2 .> length s1 ==> replace s1 s2 s3 .== s1
-- Q.E.D.
replace :: SymWord a => SSequence a -> SSequence a -> SSequence a -> SSequence a
replace s src dst
  | Just b <- unliteralList src, P.null b   -- If src is null, simply prepend
  = dst .++ s
  | Just a <- unliteralList s
  , Just b <- unliteralList src
  , Just c <- unliteralList dst
  = literalList $ walk a b c
  | True
  = lift3 (SeqOp SeqReplace) Nothing s src dst
  where walk haystack needle newNeedle = go haystack   -- note that needle is guaranteed non-empty here.
           where go []       = []
                 go i@(c:cs)
                  | needle `L.isPrefixOf` i = newNeedle ++ genericDrop (genericLength needle :: Integer) i
                  | True                    = c : go cs

-- | @`indexOf` s sub@. Retrieves first position of @sub@ in @s@, @-1@ if there are no occurrences.
-- Equivalent to @`offsetIndexOf` s sub 0@.
--
-- >>> prove $ \s i -> i .> 0 &&& i .< length s ==> indexOf s (subSeq s i 1) .<= i
-- Q.E.D.
-- >>> prove $ \s i -> i .> 0 &&& i .< length s ==> indexOf s (subSeq s i 1) .== i
-- Falsifiable. Counter-example:
--   s0 = [0, 0, 0] :: [Integer]
--   s1 =         2 :: Integer
-- >>> prove $ \s1 s2 -> length s2 .> length s1 ==> indexOf s1 s2 .== -1
-- Q.E.D.
indexOf :: SymWord a => SSequence a -> SSequence a -> SInteger
indexOf s sub = offsetIndexOf s sub 0

-- | @`offsetIndexOf` s sub offset@. Retrieves first position of @sub@ at or
-- after @offset@ in @s@, @-1@ if there are no occurrences.
--
-- >>> prove $ \s sub -> offsetIndexOf s sub 0 .== indexOf s sub
-- Q.E.D.
-- >>> prove $ \s sub i -> i .>= length s &&& length sub .> 0 ==> offsetIndexOf s sub i .== -1
-- Q.E.D.
-- >>> prove $ \s sub i -> i .> length s ==> offsetIndexOf s sub i .== -1
-- Q.E.D.
offsetIndexOf :: SymWord a => SSequence a -> SSequence a -> SInteger -> SInteger
offsetIndexOf s sub offset
  | Just c <- unliteralList s        -- a constant sequence
  , Just n <- unliteralList sub      -- a constant search pattern
  , Just o <- unliteral offset   -- at a constant offset
  , o >= 0, o <= genericLength c -- offset is good
  = case [i | (i, t) <- zip [o ..] (L.tails (genericDrop o c)), n `L.isPrefixOf` t] of
      (i:_) -> literal i
      _     -> -1
  | True
  = lift3 (SeqOp SeqIndexOf) Nothing s sub offset

-- | Is the sequence concretely known empty?
isConcretelyEmpty :: SymWord a => SSequence a -> Bool
isConcretelyEmpty ss | Just s <- unliteralList ss = P.null s
                     | True                   = False

-- | Lift a unary operator over sequences.
lift1l :: forall a b. (SymWord a, SymWord b) => Op -> Maybe ([a] -> b) -> SBV [a] -> SBV b
lift1l w mbOp a
  | Just cv <- concEval1 mbOp a
  = cv
  | True
  = SBV $ SVal k $ Right $ cache r
  where k = kindOf (undefined :: b)
        r st = do swa <- sbvToSW st a
                  newExpr st k (SBVApp w [swa])

lift1r :: forall a b. (SymWord a, SymWord b) => Op -> Maybe (a -> [b]) -> SBV a -> SBV [b]
lift1r = error "TODO(joel)"

-- | Concrete evaluation for unary ops
concEval1 :: (SymWord a, SymWord b) => Maybe ([a] -> b) -> SBV [a] -> Maybe (SBV b)
concEval1 mbOp a = literal <$> (mbOp <*> unliteralList a)

lift2 :: Op -> Maybe (a -> b -> c) -> SBV a -> SBV b -> SBV c
lift2 = error "TODO(joel)"

lift3 :: Op -> Maybe (a -> b -> c -> d) -> SBV a -> SBV b -> SBV c -> SBV d
lift3 = error "TODO(joel)"

-- | Lift a binary operator over sequences.
-- lift2 :: forall a b c. (SymWord a, SymWord b, SymWord c) => Op -> Maybe (a -> b -> c) -> SBV a -> SBV b -> SBV c
-- lift2 w mbOp a b
--   | Just cv <- concEval2 mbOp a b
--   = cv
--   | True
--   = SBV $ SVal k $ Right $ cache r
--   where k = kindOf (undefined :: c)
--         r st = do swa <- sbvToSW st a
--                   swb <- sbvToSW st b
--                   newExpr st k (SBVApp w [swa, swb])

-- -- | Lift a ternary operator over sequences.
-- lift3 :: forall a b c d. (SymWord a, SymWord b, SymWord c, SymWord d) => Op -> Maybe (a -> b -> c -> d) -> SBV a -> SBV b -> SBV c -> SBV d
-- lift3 w mbOp a b c
--   | Just cv <- concEval3 mbOp a b c
--   = cv
--   | True
--   = SBV $ SVal k $ Right $ cache r
--   where k = kindOf (undefined :: d)
--         r st = do swa <- sbvToSW st a
--                   swb <- sbvToSW st b
--                   swc <- sbvToSW st c
--                   newExpr st k (SBVApp w [swa, swb, swc])

-- | Concrete evaluation for binary ops
concEval2 :: (SymWord a, SymWord b, SymWord c) => Maybe (a -> b -> c) -> SBV a -> SBV b -> Maybe (SBV c)
concEval2 mbOp a b = literal <$> (mbOp <*> unliteral a <*> unliteral b)

-- | Concrete evaluation for ternary ops
concEval3 :: (SymWord a, SymWord b, SymWord c, SymWord d) => Maybe (a -> b -> c -> d) -> SBV a -> SBV b -> SBV c -> Maybe (SBV d)
concEval3 mbOp a b c = literal <$> (mbOp <*> unliteral a <*> unliteral b <*> unliteral c)
