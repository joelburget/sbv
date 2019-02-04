{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}

module Data.SBV.Generic where

import Data.Proxy
import GHC.Generics

import Data.SBV.Core.Data
import Data.SBV.Core.Model (genMkSymVar, mkCVTup, fromCVTup)
import Data.SBV.Core.Symbolic


class GSymbolic (f :: * -> *) where
  gKindOf :: Proxy f -> Kind

instance GSymbolic U1 where
  gKindOf _ = KTuple []

instance (GSymbolic a, GSymbolic b) => GSymbolic (a :*: b) where
  gKindOf _ = KTuple [gKindOf (Proxy @a), gKindOf (Proxy @b)]

instance (GSymbolic a, GSymbolic b) => GSymbolic (a :+: b) where
  gKindOf _ = KSum [gKindOf (Proxy @a), gKindOf (Proxy @b)]

instance GSymbolic a => GSymbolic (M1 i c a) where
  gKindOf _ = gKindOf (Proxy @a)

-- instance GSymbolic a => GSymbolic (K1 i a) where
--   gKindOf _ = gKindOf (Proxy @a)

class HasKind' a where
  kindOf' :: a -> Kind

  default kindOf' :: (Generic a, GSymbolic (Rep a)) => a -> Kind
  kindOf' _ = gKindOf (Proxy @(Rep a))


class GSymbolic f => GSymVal (f :: * -> *) where
  gMkSymVal :: MonadSymbolic m => Maybe Quantifier -> Maybe String -> m (SBV (f a))
  gLiteral :: f a -> SBV (f a)
  gFromCV :: CV -> f a

instance GSymVal U1 where
  gMkSymVal = genMkSymVar (KTuple [])
  gLiteral _ = SBV $ SVal k $ Left $ CV k $ CTuple []
    where k = KTuple []
  gFromCV = \case
    CV (KTuple []) (CTuple []) -> U1
    cv -> error $ "GSymVal U1: Unexpected CV received: " ++ show cv

gToCV :: GSymVal f => f a -> CVal
gToCV a = case gLiteral a of
  SBV (SVal _ (Left cv)) -> cvVal cv
  _ -> error "gToCV: Impossible happened, couldn't produce a concrete value"

instance (GSymVal a, GSymVal b) => GSymVal (a :*: b) where
  gMkSymVal = genMkSymVar (gKindOf (Proxy @(a :*: b)))
  gLiteral (x :*: y) = mkCVTup 2 (gKindOf (Proxy @(a :*: b))) [gToCV x, gToCV y]
  gFromCV cv = let ~[v1, v2] = fromCVTup 2 cv
               in (gFromCV v1 :*: gFromCV v2)

instance (GSymVal a, GSymVal b) => GSymVal (a :+: b) where
  gMkSymVal = genMkSymVar (gKindOf (Proxy @(a :+: b)))
  gLiteral = \case
    L1 a -> SBV $ SVal k $ Left $ CV k $ CSum 0 $ gToCV a
    R1 b -> SBV $ SVal k $ Left $ CV k $ CSum 1 $ gToCV b
    where k = gKindOf (Proxy @(a :+: b))

  gFromCV (CV (KSum [k1, _k2]) (CSum 0 c))
    = L1  $ gFromCV $ CV k1 c
  gFromCV (CV (KSum [_k1, k2]) (CSum 1 c))
    = R1 $ gFromCV $ CV k2 c
  gFromCV bad = error $ "gFromCV (Either): Malformed sum received: " ++ show bad

instance GSymVal a => GSymVal (M1 i c a) where
  gMkSymVal = genMkSymVar (gKindOf (Proxy @a))
  gLiteral (M1 _) = error "TODO" -- gLiteral a
  gFromCV = M1 . gFromCV

-- instance GSymVal a => GSymVal (K1 i a) where
--   gMkSymVal = error "TODO"
--   gLiteral = error "TODO"
--   gFromCV = error "TODO"


class SymVal' a where
  mkSymVal' :: MonadSymbolic m => Maybe Quantifier -> Maybe String -> m (SBV a)
  default mkSymVal'
    :: (Generic a, GSymVal (Rep a), MonadSymbolic m)
    => Maybe Quantifier -> Maybe String -> m (SBV a)
  mkSymVal' q nm = fmap sbvTo $ gMkSymVal q nm

  literal' :: a -> SBV a
  default literal' :: (Generic a , GSymVal (Rep a)) => a -> SBV a
  literal' = sbvTo . gLiteral . from

  fromCV' :: CV -> a
  default fromCV' :: (Generic a, GSymVal (Rep a)) => CV -> a
  fromCV' = to . gFromCV

sbvTo :: Generic a => SBV (Rep a x) -> SBV a
sbvTo = error "TODO"

instance HasKind' (Either a b)
instance SymVal' (Either a b)
-- instance (GSymVal   a, GSymVal   b) => SymVal'  (Either a b)

data Sum3 a b c
  = In1 a
  | In2 b
  | In3 c
  deriving Generic

instance HasKind' (Sum3 a b c)
-- instance (GSymVal   a, GSymVal   b, GSymVal   c) => SymVal'  (Sum3 a b c)
