module Syntax
  ( Context(..)
  , EffectMap(..)
  , Row(..)
  , Term(..)
  , Type(..)
  , VarSet(..)
  , contextLookup
  , effectMapLookup
  , freeVars
  , rowContains
  , substituteEffectInRow
  , substituteEffectInType
  , varSetContains ) where

import Test.QuickCheck
  ( Arbitrary
  , arbitrary
  , frequency
  , shrink )
import qualified Data.Set as Set

-- Data types

-- Metavariable for variable names: x
-- Metavariable for effect names: z

data Term a b -- Metavariable: e
  = EUnit
  | EVar a
  | EAbs a (Term a b)
  | EApp (Term a b) (Term a b)
  | EHandle b (Term a b) (Term a b)
  | EAnno (Term a b) (Type b)
  deriving (Eq, Show)

data Type a -- Metavariable: t
  = TUnit
  | TArrow (Type a) (Row a) (Type a) (Row a)
  deriving (Eq, Show)

data Row a -- Metavariable: r
  = REmpty
  | RSingleton a
  | RUnion (Row a) (Row a)
  deriving (Show)

data VarSet a -- Metavariable: v
  = VEmpty
  | VSingleton a
  | VUnion (VarSet a) (VarSet a)
  deriving (Show)

data Context a b -- Metavariable: c
  = CEmpty
  | CExtend (Context a b) a (Type b) (Row b)
  deriving (Eq, Show)

data EffectMap a b -- Metavariable: em
  = EMEmpty
  | EMExtend (EffectMap a b) b a
  deriving (Eq, Show)

-- Eq instances

instance Ord a => Eq (Row a) where
  (==) r1 r2 = toSet r1 == toSet r2
    where toSet REmpty = Set.empty
          toSet (RSingleton z) = Set.singleton z
          toSet (RUnion r1' r2') = Set.union (toSet r1') (toSet r2')

instance Ord a => Eq (VarSet a) where
  (==) v1 v2 = toSet v1 == toSet v2
    where toSet VEmpty = Set.empty
          toSet (VSingleton x) = Set.singleton x
          toSet (VUnion v1' v2') = Set.union (toSet v1') (toSet v2')

-- Arbitrary instances

instance (Arbitrary a, Arbitrary b) => Arbitrary (Term a b) where
  arbitrary = frequency
    [ (5, pure EUnit)
    , (5, EVar <$> arbitrary)
    , (4, EAbs <$> arbitrary <*> arbitrary)
    , (2, EApp <$> arbitrary <*> arbitrary)
    , (2, EHandle <$> arbitrary <*> arbitrary <*> arbitrary)
    , (4, EAnno <$> arbitrary <*> arbitrary) ]
  shrink EUnit = []
  shrink (EVar _) = []
  shrink (EAbs x e) = [EAbs x' e' | (x', e') <- shrink (x, e)]
  shrink (EApp e1 e2) = [EApp e1' e2' | (e1', e2') <- shrink (e1, e2)]
  shrink (EHandle z e1 e2) =
    [EHandle z' e1' e2' | (z', e1', e2') <- shrink (z, e1, e2)]
  shrink (EAnno e t) = [EAnno e' t' | (e', t') <- shrink (e, t)]

instance Arbitrary a => Arbitrary (Type a) where
  arbitrary = frequency
    [ (8, pure TUnit)
    , (3, TArrow <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary) ]
  shrink TUnit = []
  shrink (TArrow t1 r1 t2 r2) =
    [TArrow t1' r1' t2' r2' | (t1', r1', t2', r2') <- shrink (t1, r1, t2, r2)]

instance Arbitrary a => Arbitrary (Row a) where
  arbitrary = frequency
    [ (5, pure REmpty)
    , (5, RSingleton <$> arbitrary)
    , (3, RUnion <$> arbitrary <*> arbitrary) ]
  shrink REmpty = []
  shrink (RSingleton _) = []
  shrink (RUnion r1 r2) =
    [r1, r2] ++ [RUnion r1' r2' | (r1', r2') <- shrink (r1, r2)]

instance Arbitrary a => Arbitrary (VarSet a) where
  arbitrary = frequency
    [ (5, pure VEmpty)
    , (5, VSingleton <$> arbitrary)
    , (3, VUnion <$> arbitrary <*> arbitrary) ]
  shrink VEmpty = []
  shrink (VSingleton _) = []
  shrink (VUnion v1 v2) =
    [v1, v2] ++ [VUnion v1' v2' | (v1', v2') <- shrink (v1, v2)]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Context a b) where
  arbitrary = frequency
    [ (1, pure CEmpty)
    , (3, CExtend <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary) ]
  shrink CEmpty = []
  shrink (CExtend c x t r) =
    [CExtend c' x' t' r' | (c', x', t', r') <- shrink (c, x, t, r)]

instance (Arbitrary a, Arbitrary b) => Arbitrary (EffectMap a b) where
  arbitrary = frequency
    [ (1, pure EMEmpty)
    , (3, EMExtend
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary) ]
  shrink EMEmpty = []
  shrink (EMExtend c z x) =
    [EMExtend c' z' x' | (c', z', x') <- shrink (c, z, x)]

-- Helper functions

contextLookup :: Eq a => Context a b -> a -> Maybe (Type b, Row b)
contextLookup CEmpty _ = Nothing
contextLookup (CExtend c x1 t r) x2 =
  if x1 == x2
  then Just (t, r)
  else contextLookup c x2

effectMapLookup :: Eq b => EffectMap a b -> b -> Maybe a
effectMapLookup EMEmpty _ = Nothing
effectMapLookup (EMExtend em z1 x) z2 =
  if z1 == z2
  then Just x
  else effectMapLookup em z2

freeVars :: Ord a => Term a b -> VarSet a
freeVars e = foldr (\x v -> VUnion (VSingleton x) v) VEmpty (freeVarsSet e)

freeVarsSet :: Ord a => Term a b -> Set.Set a
freeVarsSet EUnit = Set.empty
freeVarsSet (EVar x) = Set.singleton x
freeVarsSet (EAbs x e) = Set.delete x (freeVarsSet e)
freeVarsSet (EApp e1 e2) = Set.union (freeVarsSet e1) (freeVarsSet e2)
freeVarsSet (EHandle _ e1 e2) = Set.union (freeVarsSet e1) (freeVarsSet e2)
freeVarsSet (EAnno e _) = freeVarsSet e

substituteEffectInRow :: Eq a => a -> Row a -> Row a -> Row a
substituteEffectInRow _ _ REmpty = REmpty
substituteEffectInRow z1 r (RSingleton z2) =
  if z1 == z2
  then r
  else RSingleton z2
substituteEffectInRow z r1 (RUnion r2 r3) =
  RUnion (substituteEffectInRow z r1 r2) (substituteEffectInRow z r1 r3)

substituteEffectInType :: Eq a => a -> Row a -> Type a -> Type a
substituteEffectInType _ _ TUnit = TUnit
substituteEffectInType z r3 (TArrow t1 r1 t2 r2) =
  TArrow
    (substituteEffectInType z r3 t1)
    (substituteEffectInRow z r3 r1)
    (substituteEffectInType z r3 t2)
    (substituteEffectInRow z r3 r2)

rowContains :: Eq a => a -> Row a -> Bool
rowContains _ REmpty = False
rowContains x1 (RSingleton x2) = x1 == x2
rowContains x (RUnion r1 r2) = rowContains x r1 || rowContains x r2

varSetContains :: Eq a => a -> VarSet a -> Bool
varSetContains _ VEmpty = False
varSetContains x2 (VSingleton x1) = x1 == x2
varSetContains x (VUnion v1 v2) = varSetContains x v1 || varSetContains x v2
