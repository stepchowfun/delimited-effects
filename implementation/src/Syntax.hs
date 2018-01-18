module Syntax
  ( Context(..)
  , EffectMap(..)
  , HoistedSet(..)
  , Row(..)
  , Term(..)
  , Type(..)
  , contextLookupKind
  , contextLookupType
  , eff
  , effectMapLookup
  , effects
  , ftv
  , fv
  , rowContains
  , substituteEffectInRow
  , substituteEffectInType
  , substituteTypeInType ) where

import Test.QuickCheck
  ( Arbitrary
  , arbitrary
  , frequency
  , Gen
  , oneof
  , shrink )
import qualified Data.Set as Set

-- Data types

-- Metavariable for variable names: x
-- Metavariable for effect names: z

data Term -- Metavariable: e
  = EVar String
  | EAbs String Term
  | EApp Term Term
  | ETAbs String Term
  | ETApp Term Type
  | EHandle String Term Term
  | EAnno Term Type
  deriving (Eq, Show)

data Type -- Metavariable: t
  = TVar String
  | TArrow Type Row Type Row
  | TForall String Type Row
  deriving (Eq, Show)

data Row -- Metavariable: r
  = REmpty
  | RSingleton String
  | RUnion Row Row
  deriving (Show)

data HoistedSet -- Metavariable: h
  = HEmpty
  | HSingleton Term Type Row
  | HUnion HoistedSet HoistedSet
  deriving (Show)

data Context -- Metavariable: c
  = CEmpty
  | CTExtend Context String Type Row
  | CKExtend Context String
  deriving (Eq, Show)

data EffectMap -- Metavariable: em
  = EMEmpty
  | EMExtend EffectMap String String
  deriving (Eq, Show)

-- Eq instances

instance Eq Row where
  (==) r1 r2 = toSet r1 == toSet r2
    where toSet REmpty = Set.empty
          toSet (RSingleton z) = Set.singleton z
          toSet (RUnion r1' r2') = Set.union (toSet r1') (toSet r2')

-- Arbitrary instances
effects :: [String]
effects = ["EffectA", "EffectB", "EffectC", "EffectD", "EffectE"]

arbitraryEffect :: Gen String
arbitraryEffect = oneof $ map (\e -> return e) effects

termVars :: [String]
termVars = ["TermVarV", "TermVarW", "TermVarX", "TermVarY", "TermVarZ"]

arbitraryTermVar :: Gen String
arbitraryTermVar = oneof $ map (\e -> return e) termVars

typeVars :: [String]
typeVars = ["TypeVarV", "TypeVarW", "TypeVarX", "TypeVarY", "TypeVarZ"]

arbitraryTypeVar :: Gen String
arbitraryTypeVar = oneof $ map (\e -> return e) typeVars

instance Arbitrary Term where
  arbitrary = frequency
    [ (5, EVar <$> arbitraryTermVar)
    , (4, EAbs <$> arbitraryTermVar <*> arbitrary)
    , (2, EApp <$> arbitrary <*> arbitrary)
    , (4, ETAbs <$> arbitraryTypeVar <*> arbitrary)
    , (2, ETApp <$> arbitrary <*> arbitrary)
    , (2, EHandle <$> arbitraryEffect <*> arbitrary <*> arbitrary)
    , (4, EAnno <$> arbitrary <*> arbitrary) ]
  shrink (EVar _) = []
  shrink (EAbs x e) = [EAbs x' e' | (x', e') <- shrink (x, e)]
  shrink (EApp e1 e2) = [EApp e1' e2' | (e1', e2') <- shrink (e1, e2)]
  shrink (ETAbs a e) = [ETAbs a' e' | (a', e') <- shrink (a, e)]
  shrink (ETApp e1 e2) = [ETApp e1' e2' | (e1', e2') <- shrink (e1, e2)]
  shrink (EHandle z e1 e2) =
    [EHandle z' e1' e2' | (z', e1', e2') <- shrink (z, e1, e2)]
  shrink (EAnno e t) = [EAnno e' t' | (e', t') <- shrink (e, t)]

instance Arbitrary Type where
  arbitrary = frequency
    [ (16, TVar <$> arbitraryTypeVar)
    , (3, TArrow <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)
    , (3, TForall <$> arbitraryTypeVar <*> arbitrary <*> arbitrary) ]
  shrink (TVar _) = []
  shrink (TArrow t1 r1 t2 r2) =
    [TArrow t1' r1' t2' r2' | (t1', r1', t2', r2') <- shrink (t1, r1, t2, r2)]
  shrink (TForall a t r) =
    [TForall a' t' r' | (a', t', r') <- shrink (a, t, r)]

instance Arbitrary Row where
  arbitrary = frequency
    [ (5, pure REmpty)
    , (5, RSingleton <$> arbitraryEffect)
    , (3, RUnion <$> arbitrary <*> arbitrary) ]
  shrink REmpty = []
  shrink (RSingleton _) = []
  shrink (RUnion r1 r2) =
    [r1, r2] ++ [RUnion r1' r2' | (r1', r2') <- shrink (r1, r2)]

instance Arbitrary HoistedSet where
  arbitrary = frequency
    [ (5, pure HEmpty)
    , (5, HSingleton <$> arbitrary <*> arbitrary <*> arbitrary)
    , (3, HUnion <$> arbitrary <*> arbitrary) ]
  shrink HEmpty = []
  shrink (HSingleton _ _ _) = []
  shrink (HUnion h1 h2) =
    [h1, h2] ++ [HUnion h1' h2' | (h1', h2') <- shrink (h1, h2)]

instance Arbitrary Context where
  arbitrary = frequency
    [ (2, pure CEmpty)
    , (3, CTExtend
        <$> arbitrary
        <*> arbitraryTermVar
        <*> arbitrary
        <*> arbitrary)
    , (3, CKExtend <$> arbitrary <*> arbitraryTypeVar) ]
  shrink CEmpty = []
  shrink (CTExtend c x t r) =
    [CTExtend c' x' t' r' | (c', x', t', r') <- shrink (c, x, t, r)]
  shrink (CKExtend c a) =
    [CKExtend c' a' | (c', a') <- shrink (c, a)]

instance Arbitrary EffectMap where
  arbitrary = frequency
    [ (1, pure EMEmpty)
    , (3, EMExtend
        <$> arbitrary
        <*> arbitraryEffect
        <*> arbitraryTermVar) ]
  shrink EMEmpty = []
  shrink (EMExtend c z x) =
    [EMExtend c' z' x' | (c', z', x') <- shrink (c, z, x)]

-- Helper functions

contextLookupType :: Context -> String -> Maybe (Type, Row)
contextLookupType CEmpty _ = Nothing
contextLookupType (CTExtend c x1 t r) x2 =
  if x1 == x2 then Just (t, r) else contextLookupType c x2
contextLookupType (CKExtend c _) x2 = contextLookupType c x2

contextLookupKind :: Context -> String -> Maybe ()
contextLookupKind CEmpty             _  = Nothing
contextLookupKind (CTExtend c _ _ _) a2 = contextLookupKind c a2
contextLookupKind (CKExtend c a1) a2 =
  if a1 == a2 then Just () else contextLookupKind c a2

effectMapLookup :: EffectMap -> String -> Maybe String
effectMapLookup EMEmpty _ = Nothing
effectMapLookup (EMExtend em z1 x) z2 =
  if z1 == z2 then Just x else effectMapLookup em z2

eff :: HoistedSet -> Row
eff HEmpty             = REmpty
eff (HSingleton _ _ r) = r
eff (HUnion h1 h2    ) = RUnion (eff h1) (eff h2)

fv :: HoistedSet -> Set.Set String
fv HEmpty             = Set.empty
fv (HSingleton e _ _) = freeTermVarsInTerm e
fv (HUnion h1 h2    ) = Set.union (fv h1) (fv h2)

freeTermVarsInTerm :: Term -> Set.Set String
freeTermVarsInTerm (EVar x  ) = Set.singleton x
freeTermVarsInTerm (EAbs x e) = Set.delete x (freeTermVarsInTerm e)
freeTermVarsInTerm (EApp e1 e2) =
  Set.union (freeTermVarsInTerm e1) (freeTermVarsInTerm e2)
freeTermVarsInTerm (ETAbs _ e) = freeTermVarsInTerm e
freeTermVarsInTerm (ETApp e _) = freeTermVarsInTerm e
freeTermVarsInTerm (EHandle _ e1 e2) =
  Set.union (freeTermVarsInTerm e1) (freeTermVarsInTerm e2)
freeTermVarsInTerm (EAnno e _) = freeTermVarsInTerm e

ftv :: HoistedSet -> Set.Set String
ftv HEmpty = Set.empty
ftv (HSingleton e t _) =
  Set.union (freeTypeVarsInTerm e) (freeTypeVarsInType t)
ftv (HUnion h1 h2) = Set.union (ftv h1) (ftv h2)

freeTypeVarsInTerm :: Term -> Set.Set String
freeTypeVarsInTerm (EVar _  ) = Set.empty
freeTypeVarsInTerm (EAbs _ e) = freeTypeVarsInTerm e
freeTypeVarsInTerm (EApp e1 e2) =
  Set.union (freeTypeVarsInTerm e1) (freeTypeVarsInTerm e2)
freeTypeVarsInTerm (ETAbs a e) = Set.delete a (freeTypeVarsInTerm e)
freeTypeVarsInTerm (ETApp e t) =
  Set.union (freeTypeVarsInTerm e) (freeTypeVarsInType t)
freeTypeVarsInTerm (EHandle _ e1 e2) =
  Set.union (freeTypeVarsInTerm e1) (freeTypeVarsInTerm e2)
freeTypeVarsInTerm (EAnno e t) =
  Set.union (freeTypeVarsInTerm e) (freeTypeVarsInType t)

freeTypeVarsInType :: Type -> Set.Set String
freeTypeVarsInType (TVar a) = Set.singleton a
freeTypeVarsInType (TArrow t1 _ t2 _) =
  Set.union (freeTypeVarsInType t1) (freeTypeVarsInType t2)
freeTypeVarsInType (TForall a t _) = Set.delete a (freeTypeVarsInType t)

substituteEffectInRow :: String -> Row -> Row -> Row
substituteEffectInRow _ _ REmpty = REmpty
substituteEffectInRow z1 r (RSingleton z2) =
  if z1 == z2 then r else RSingleton z2
substituteEffectInRow z r1 (RUnion r2 r3) =
  RUnion (substituteEffectInRow z r1 r2) (substituteEffectInRow z r1 r3)

substituteEffectInType :: String -> Row -> Type -> Type
substituteEffectInType _ _  (TVar a            ) = TVar a
substituteEffectInType z r3 (TArrow t1 r1 t2 r2) = TArrow
  (substituteEffectInType z r3 t1)
  (substituteEffectInRow z r3 r1)
  (substituteEffectInType z r3 t2)
  (substituteEffectInRow z r3 r2)
substituteEffectInType z r1 (TForall a t r2) =
  TForall a (substituteEffectInType z r1 t) (substituteEffectInRow z r1 r2)

substituteTypeInType :: String -> Type -> Type -> Type
substituteTypeInType a1 t (TVar a2) = if a1 == a2 then t else (TVar a2)
substituteTypeInType a t3 (TArrow t1 r1 t2 r2) =
  TArrow (substituteTypeInType a t3 t1) r1 (substituteTypeInType a t3 t2) r2
substituteTypeInType a1 t1 (TForall a2 t2 r) = if a1 == a2
  then TForall a2 t2 r
  else TForall a2 (substituteTypeInType a1 t1 t2) r

rowContains :: String -> Row -> Bool
rowContains _  REmpty          = False
rowContains x1 (RSingleton x2) = x1 == x2
rowContains x  (RUnion r1 r2 ) = rowContains x r1 || rowContains x r2
