-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.List
-- Copyright   :  (c) Joel Burget, Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- A collection of list utilities, useful when working with symbolic lists.
-- To the extent possible, the functions in this module follow those of "Data.List"
-- so importing qualified is the recommended workflow. Also, it is recommended
-- you use the @OverloadedLists@ extension to allow literal lists to
-- be used as symbolic-lists.
-----------------------------------------------------------------------------

{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.SBV.List (
        head, tail, nil, cons, uncons, singleton, implode, (.:), elemAt, (.!!), length, concat, (.++), foldr, sum, product, and, or
        ) where

import Prelude hiding (head, tail, length, concat, foldr, sum, product, and, or)
import qualified Prelude as P

import Data.SBV.Core.Data hiding (StrOp(..))
import Data.SBV.Core.Model
import Data.SBV.Utils.Boolean

import Debug.Trace

-- For doctest use only
--
-- $setup
-- >>> import Data.SBV.Provers.Prover (prove, sat)
-- >>> import Data.SBV.Utils.Boolean  ((==>), (&&&), bnot, (<=>))
-- >>> import Data.Int
-- >>> import Data.Word
-- >>> :set -XOverloadedLists
-- >>> :set -XScopedTypeVariables

nil :: SymWord a => SList a
nil = literal []

cons :: SymWord a => SBV a -> SList a -> SList a
cons a as
  -- | Just a'  <- trace "unliteral a" $ unliteral a
  -- , Just as' <- trace "unliteral as" $ unliteral as
  -- = trace "literal cons" $ literal (a' : as')
  | True
  = lift2 ListCons Nothing a as

-- | @`head`@ returns the first element of a list. Unspecified if the list is empty.
--
-- >>> prove $ \c -> head (singleton c) .== (c :: SInteger)
-- Q.E.D.
head :: SymWord a => SList a -> SBV a
head l
  | Just (x:_) <- unliteral l
  = literal x
  | True
  = lift1 ListHead Nothing l

-- | @`tail`@ returns the tail of a list. Unspecified if the list is empty.
--
-- >>> prove $ \(h :: SInteger) t -> tail (singleton h .++ t) .== t
-- Q.E.D.
-- >>> prove $ \(l :: SList Integer) -> length l .> 0 ==> length (tail l) .== length l - 1
-- Q.E.D.
-- >>> prove $ \(l :: SList Integer) -> bnot (null l) ==> singleton (head l) .++ tail l .== l
-- Q.E.D.
tail :: SymWord a => SList a -> SList a
tail l
 | Just (_:cs) <- unliteral l
 = literal cs
 | True
 = lift1 ListTail Nothing l

-- | @`uncons` returns the pair of the head and tail. Unspecified if the list is empty.
uncons :: SymWord a => SList a -> (SBV a, SList a)
uncons l = (head l, tail l)

-- | @`singleton` x@ is the list of length 1 that contains the only value `x`.
--
-- >>> prove $ \(x :: SInteger) -> head (singleton x) .== x
-- Q.E.D.
-- >>> prove $ \(x :: SInteger) -> length (singleton x) .== 1
-- Q.E.D.
singleton :: SymWord a => SBV a -> SList a
singleton = (.: nil)

-- | @`implode` es@ is the list of length @|es|@ containing precisely those
-- elements. Note that there is no corresponding function @explode@, since
-- we wouldn't know the length of a symbolic list.
--
-- >>> prove $ \(e1 :: SInteger) e2 e3 -> length (implode [e1, e2, e3]) .== 3
-- Q.E.D.
-- >>> prove $ \(e1 :: SInteger) e2 e3 -> map (elemAt (implode [e1, e2, e3])) (map literal [0 .. 2]) .== [e1, e2, e3]
-- Q.E.D.
implode :: SymWord a => [SBV a] -> SList a
implode = P.foldr cons nil

-- | Prepend an element, the traditional @cons@.
infixr 5 .:
(.:) :: SymWord a => SBV a -> SList a -> SList a
(.:) = cons

elemAt :: forall a. SymWord a => SList a -> SInteger -> SBV a
elemAt xs i = ite (i .== 0) (head xs) (elemAt (tail xs) (i - 1))

(.!!) :: forall a. SymWord a => SList a -> SInteger -> SBV a
(.!!) = elemAt

length :: forall a. SymWord a => SList a -> SInteger
-- length xs = ite (xs .== nil) 0 (1 + length (tail xs))
length xs =
  let r st = do swxs <- sbvToSW st xs
                traceM $ "length swxs: " ++ show swxs
                newExpr st KUnbounded (SBVApp (ListOp ListLength) [swxs])
  in SBV $ SVal KUnbounded $ Right $ cache r

concat :: forall a. SymWord a => SList a -> SList a -> SList a
concat xs ys = ite (xs .== nil) ys (cons (head xs) (concat (tail xs) ys))

(.++) :: forall a. SymWord a => SList a -> SList a -> SList a
(.++) = concat

-- TODO: way to define a z3 function with included hypotheses
foldr
  :: forall a b. (SymWord a, SymWord b)
  => (SBV a -> SBV b -> SBV b) -> SBV b -> SList a -> SBV b
foldr f b as = ite (as .== nil) b (f (head as) (foldr f b (tail as)))

sum :: forall a. (SymWord a, Num a) => SList a -> SBV a
sum xs =
  let r st = do swxs <- sbvToSW st xs
                traceM $ "sum swxs: " ++ show swxs
                newExpr st KUnbounded (SBVApp (ListOp ListSum) [swxs])
  in SBV $ SVal KUnbounded $ Right $ cache r

product :: forall a. (SymWord a, Num a) => SList a -> SBV a
product = foldr (*) 0

and :: SList Bool -> SBool
and = foldr (&&&) true

or :: SList Bool -> SBool
or = foldr (|||) false

-- all :: forall a. SymWord a -> (SBV a -> SBool) -> SList a -> Bool
-- any
-- map
-- filter

lift1 :: forall a b. (SymWord a, SymWord b) => ListOp -> Maybe (a -> b) -> SBV a -> SBV b
lift1 w mbOp a
  | Just cv <- concEval1 mbOp a
  = cv
  | True
  = SBV $ SVal k $ Right $ cache r
  where k = kindOf (undefined :: b)
        r st = do swa <- sbvToSW st a
                  newExpr st k (SBVApp (ListOp w) [swa])

-- | Concrete evaluation for unary ops
concEval1 :: (SymWord a, SymWord b) => Maybe (a -> b) -> SBV a -> Maybe (SBV b)
concEval1 mbOp a = literal <$> (mbOp <*> unliteral a)

lift2 :: forall a b c. (SymWord a, SymWord b, SymWord c) => ListOp -> Maybe (a -> b -> c) -> SBV a -> SBV b -> SBV c
lift2 w mbOp a b
  -- | Just cv <- concEval2 mbOp a b
  -- = cv
  | True
  = SBV $ SVal k $ Right $ cache r
  where k = kindOf (undefined :: c)
        r st = do swa <- sbvToSW st a
                  swb <- sbvToSW st b
                  newExpr st k (SBVApp (ListOp w) [swa, swb])

-- | Concrete evaluation for binary ops
concEval2 :: (SymWord a, SymWord b, SymWord c) => Maybe (a -> b -> c) -> SBV a -> SBV b -> Maybe (SBV c)
concEval2 mbOp a b = literal <$> (mbOp <*> unliteral a <*> unliteral b)
