{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Syntax
  ( BBigLambda(..)
  , BForAll(..)
  , BSmallLambda(..)
  , EVarName(..)
  , FTerm(..)
  , FreeEVars
  , FreeTCons
  , FreeTVars
  , ITerm(..)
  , Subst
  , TConName(..)
  , TVarName(..)
  , Type(..)
  , arrowName
  , arrowType
  , boolName
  , boolType
  , collectBinders
  , freeEVars
  , freeTCons
  , freeTVars
  , intName
  , intType
  , listName
  , listType
  , propagate
  , subst
  ) where

import Data.Function (on)
import Data.List (groupBy, intercalate, nub, unwords)
import Data.Set (Set)
import qualified Data.Set as Set

-- Core data types
newtype EVarName =
  EVarName String
  deriving (Eq, Ord)

instance Show EVarName where
  show (EVarName s) = s

data TVarName
  = UserTVarName String -- Can be referred to in source programs
  | AutoTVarName Integer -- Cannot be referred to in source programs
  deriving (Eq, Ord)

instance Show TVarName where
  show (UserTVarName s) = s
  show (AutoTVarName i) = "$" ++ show i

data TConName
  = UserTConName String -- Can be referred to in source programs
  | AutoTConName Integer -- Cannot be referred to in source programs
  deriving (Eq, Ord)

instance Show TConName where
  show (UserTConName s) = s
  show (AutoTConName i) = "%" ++ show i

data ITerm
  = IEVar EVarName
  | IEAbs EVarName
          Type
          ITerm
  | IEApp ITerm
          ITerm
  | IELet EVarName
          ITerm
          ITerm
  | IEAnno ITerm
           Type
  | IETrue
  | IEFalse
  | IEIf ITerm
         ITerm
         ITerm
  | IEIntLit Integer
  | IEAdd ITerm
          ITerm
  | IESub ITerm
          ITerm
  | IEMul ITerm
          ITerm
  | IEDiv ITerm
          ITerm
  | IEList [ITerm]
  | IEConcat ITerm
             ITerm

data FTerm
  = FEVar EVarName
  | FEAbs EVarName
          Type
          FTerm
  | FEApp FTerm
          FTerm
  | FETAbs TVarName
           FTerm
  | FETApp FTerm
           Type
  | FETrue
  | FEFalse
  | FEIf FTerm
         FTerm
         FTerm
  | FEIntLit Integer
  | FEAdd FTerm
          FTerm
  | FESub FTerm
          FTerm
  | FEMul FTerm
          FTerm
  | FEDiv FTerm
          FTerm
  | FEList [FTerm]
  | FEConcat FTerm
             FTerm

data Type
  = TVar TVarName
  | TCon TConName
         [Type]
  | TForAll TVarName
            Type

-- Built-in type constructors
boolName :: TConName
boolName = UserTConName "Bool"

boolType :: Type
boolType = TCon boolName []

intName :: TConName
intName = UserTConName "Int"

intType :: Type
intType = TCon intName []

arrowName :: TConName
arrowName = UserTConName "Arrow"

arrowType :: Type -> Type -> Type
arrowType t1 t2 = TCon arrowName [t1, t2]

listName :: TConName
listName = UserTConName "List"

listType :: Type -> Type
listType t = TCon listName [t]

-- Free variables
class FreeEVars a where
  freeEVars :: a -> [EVarName]

-- The freeTVars function should return type variables in the order they first
-- appear. This is important for unification of types with universal
-- quantifiers.
class FreeTVars a where
  freeTVars :: a -> [TVarName]

class FreeTCons a where
  freeTCons :: a -> [TConName]

instance FreeEVars ITerm where
  freeEVars (IEVar x) = [x]
  freeEVars (IEAbs x _ e) = filter (/= x) (freeEVars e)
  freeEVars (IEApp e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (IELet x e1 e2) =
    nub $ freeEVars e1 ++ filter (/= x) (freeEVars e2)
  freeEVars (IEAnno e _) = freeEVars e
  freeEVars IETrue = []
  freeEVars IEFalse = []
  freeEVars (IEIf e1 e2 e3) =
    nub $ freeEVars e1 ++ freeEVars e2 ++ freeEVars e3
  freeEVars (IEIntLit _) = []
  freeEVars (IEAdd e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (IESub e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (IEMul e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (IEDiv e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (IEList es) = nub $ es >>= freeEVars
  freeEVars (IEConcat e1 e2) = nub $ freeEVars e1 ++ freeEVars e2

-- We deliberately omit a FreeTVars ITerm instance. The only free type
-- variables of an ITerm come from type annotations, but free type variables in
-- annotations are interpreted as implicitly existentially bound (i.e., they
-- aren't really free).
instance FreeTCons ITerm where
  freeTCons (IEVar _) = []
  freeTCons (IEAbs _ t e) = nub $ freeTCons t ++ freeTCons e
  freeTCons (IEApp e1 e2) = nub $ freeTCons e1 ++ freeTCons e2
  freeTCons (IELet _ e1 e2) = nub $ freeTCons e1 ++ freeTCons e2
  freeTCons (IEAnno e t) = nub $ freeTCons e ++ freeTCons t
  freeTCons IETrue = []
  freeTCons IEFalse = []
  freeTCons (IEIf e1 e2 e3) =
    nub $ freeTCons e1 ++ freeTCons e2 ++ freeTCons e3
  freeTCons (IEIntLit _) = []
  freeTCons (IEAdd e1 e2) = nub $ freeTCons e1 ++ freeTCons e2
  freeTCons (IESub e1 e2) = nub $ freeTCons e1 ++ freeTCons e2
  freeTCons (IEMul e1 e2) = nub $ freeTCons e1 ++ freeTCons e2
  freeTCons (IEDiv e1 e2) = nub $ freeTCons e1 ++ freeTCons e2
  freeTCons (IEList es) = nub $ es >>= freeTCons
  freeTCons (IEConcat e1 e2) = nub $ freeTCons e1 ++ freeTCons e2

instance FreeEVars FTerm where
  freeEVars (FEVar x) = [x]
  freeEVars (FEAbs x _ e) = filter (/= x) (freeEVars e)
  freeEVars (FEApp e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (FETAbs _ e) = freeEVars e
  freeEVars (FETApp e _) = freeEVars e
  freeEVars FETrue = []
  freeEVars FEFalse = []
  freeEVars (FEIf e1 e2 e3) =
    nub $ freeEVars e1 ++ freeEVars e2 ++ freeEVars e3
  freeEVars (FEIntLit _) = []
  freeEVars (FEAdd e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (FESub e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (FEMul e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (FEDiv e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (FEList es) = nub $ es >>= freeEVars
  freeEVars (FEConcat e1 e2) = nub $ freeEVars e1 ++ freeEVars e2

instance FreeTVars FTerm where
  freeTVars (FEVar _) = []
  freeTVars (FEAbs _ t e) = nub $ freeTVars t ++ freeTVars e
  freeTVars (FEApp e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (FETAbs a e) = filter (/= a) (freeTVars e)
  freeTVars (FETApp e t) = nub $ freeTVars e ++ freeTVars t
  freeTVars FETrue = []
  freeTVars FEFalse = []
  freeTVars (FEIf e1 e2 e3) =
    nub $ freeTVars e1 ++ freeTVars e2 ++ freeTVars e3
  freeTVars (FEIntLit _) = []
  freeTVars (FEAdd e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (FESub e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (FEMul e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (FEDiv e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (FEList es) = nub $ es >>= freeTVars
  freeTVars (FEConcat e1 e2) = nub $ freeTVars e1 ++ freeTVars e2

instance FreeTCons FTerm where
  freeTCons (FEVar _) = []
  freeTCons (FEAbs _ t e) = nub $ freeTCons t ++ freeTCons e
  freeTCons (FEApp e1 e2) = nub $ freeTCons e1 ++ freeTCons e2
  freeTCons (FETAbs _ e) = freeTCons e
  freeTCons (FETApp e t) = nub $ freeTCons e ++ freeTCons t
  freeTCons FETrue = []
  freeTCons FEFalse = []
  freeTCons (FEIf e1 e2 e3) =
    nub $ freeTCons e1 ++ freeTCons e2 ++ freeTCons e3
  freeTCons (FEIntLit _) = []
  freeTCons (FEAdd e1 e2) = nub $ freeTCons e1 ++ freeTCons e2
  freeTCons (FESub e1 e2) = nub $ freeTCons e1 ++ freeTCons e2
  freeTCons (FEMul e1 e2) = nub $ freeTCons e1 ++ freeTCons e2
  freeTCons (FEDiv e1 e2) = nub $ freeTCons e1 ++ freeTCons e2
  freeTCons (FEConcat e1 e2) = nub $ freeTCons e1 ++ freeTCons e2
  freeTCons (FEList es) = nub $ es >>= freeTCons

instance FreeTVars Type where
  freeTVars (TVar a) = [a]
  freeTVars (TCon _ ts) = foldr (\t as -> freeTVars t ++ as) [] ts
  freeTVars (TForAll a t) = filter (/= a) (freeTVars t)

instance FreeTCons Type where
  freeTCons (TVar _) = []
  freeTCons (TCon c ts) = c : foldr (\t cs -> freeTCons t ++ cs) [] ts
  freeTCons (TForAll _ t) = freeTCons t

-- Substitution
class Subst a b c where
  subst :: a -> b -> c -> c

instance Subst EVarName ITerm ITerm where
  subst x1 e (IEVar x2) =
    if x1 == x2
      then e
      else IEVar x2
  subst x1 e1 (IEAbs x2 t e2) =
    IEAbs x2 t $
    if x1 == x2
      then e2
      else subst x1 e1 e2
  subst x e1 (IEApp e2 e3) = IEApp (subst x e1 e2) (subst x e1 e3)
  subst x1 e1 (IELet x2 e2 e3) =
    IELet
      x2
      (subst x1 e1 e2)
      (if x1 == x2
         then e3
         else subst x1 e1 e3)
  subst x e1 (IEAnno e2 t) = IEAnno (subst x e1 e2) t
  subst _ _ IETrue = IETrue
  subst _ _ IEFalse = IEFalse
  subst x e1 (IEIf e2 e3 e4) =
    IEIf (subst x e1 e2) (subst x e1 e3) (subst x e1 e4)
  subst _ _ (IEIntLit i) = IEIntLit i
  subst x e1 (IEAdd e2 e3) = IEAdd (subst x e1 e2) (subst x e1 e3)
  subst x e1 (IESub e2 e3) = IESub (subst x e1 e2) (subst x e1 e3)
  subst x e1 (IEMul e2 e3) = IEMul (subst x e1 e2) (subst x e1 e3)
  subst x e1 (IEDiv e2 e3) = IEDiv (subst x e1 e2) (subst x e1 e3)
  subst x e (IEList es) = IEList $ subst x e <$> es
  subst x e1 (IEConcat e2 e3) = IEConcat (subst x e1 e2) (subst x e1 e3)

-- We deliberately omit a Subst TVarName Type ITerm instance. The only free
-- type variables of an ITerm come from type annotations, but free type
-- variables in annotations are interpreted as implicitly existentially bound
-- (i.e., they aren't really free).
instance Subst TConName Type ITerm where
  subst _ _ (IEVar x) = IEVar x
  subst c t1 (IEAbs x t2 e) = IEAbs x (subst c t1 t2) (subst c t1 e)
  subst c t (IEApp e1 e2) = IEApp (subst c t e1) (subst c t e2)
  subst c t (IELet x e1 e2) = IELet x (subst c t e1) (subst c t e2)
  subst c t1 (IEAnno e t2) = IEAnno (subst c t1 e) (subst c t1 t2)
  subst _ _ IETrue = IETrue
  subst _ _ IEFalse = IEFalse
  subst c t (IEIf e1 e2 e3) =
    IEIf (subst c t e1) (subst c t e2) (subst c t e3)
  subst _ _ (IEIntLit i) = IEIntLit i
  subst c t (IEAdd e1 e2) = IEAdd (subst c t e1) (subst c t e2)
  subst c t (IESub e1 e2) = IESub (subst c t e1) (subst c t e2)
  subst c t (IEMul e1 e2) = IEMul (subst c t e1) (subst c t e2)
  subst c t (IEDiv e1 e2) = IEDiv (subst c t e1) (subst c t e2)
  subst c e (IEList es) = IEList $ subst c e <$> es
  subst c t (IEConcat e1 e2) = IEConcat (subst c t e1) (subst c t e2)

instance Subst EVarName FTerm FTerm where
  subst x1 e (FEVar x2) =
    if x1 == x2
      then e
      else FEVar x2
  subst x1 e1 (FEAbs x2 t e2) =
    FEAbs x2 t $
    if x1 == x2
      then e2
      else subst x1 e1 e2
  subst x e1 (FEApp e2 e3) = FEApp (subst x e1 e2) (subst x e1 e3)
  subst x e1 (FETAbs a e2) = FETAbs a (subst x e1 e2)
  subst x e1 (FETApp e2 t) = FETApp (subst x e1 e2) t
  subst _ _ FETrue = FETrue
  subst _ _ FEFalse = FEFalse
  subst x e1 (FEIf e2 e3 e4) =
    FEIf (subst x e1 e2) (subst x e1 e3) (subst x e1 e4)
  subst _ _ (FEIntLit i) = FEIntLit i
  subst x e1 (FEAdd e2 e3) = FEAdd (subst x e1 e2) (subst x e1 e3)
  subst x e1 (FESub e2 e3) = FESub (subst x e1 e2) (subst x e1 e3)
  subst x e1 (FEMul e2 e3) = FEMul (subst x e1 e2) (subst x e1 e3)
  subst x e1 (FEDiv e2 e3) = FEDiv (subst x e1 e2) (subst x e1 e3)
  subst x e (FEList es) = FEList $ subst x e <$> es
  subst x e1 (FEConcat e2 e3) = FEConcat (subst x e1 e2) (subst x e1 e3)

instance Subst TVarName Type FTerm where
  subst _ _ (FEVar x) = FEVar x
  subst a t1 (FEAbs x t2 e) = FEAbs x (subst a t1 t2) (subst a t1 e)
  subst a t (FEApp e1 e2) = FEApp (subst a t e1) (subst a t e2)
  subst a1 t (FETAbs a2 e) = FETAbs a2 (subst a1 t e)
  subst a t1 (FETApp e t2) = FETApp (subst a t1 e) (subst a t1 t2)
  subst _ _ FETrue = FETrue
  subst _ _ FEFalse = FEFalse
  subst a t (FEIf e1 e2 e3) =
    FEIf (subst a t e1) (subst a t e2) (subst a t e3)
  subst _ _ (FEIntLit i) = FEIntLit i
  subst a t (FEAdd e1 e2) = FEAdd (subst a t e1) (subst a t e2)
  subst a t (FESub e1 e2) = FESub (subst a t e1) (subst a t e2)
  subst a t (FEMul e1 e2) = FEMul (subst a t e1) (subst a t e2)
  subst a t (FEDiv e1 e2) = FEDiv (subst a t e1) (subst a t e2)
  subst a e (FEList es) = FEList $ subst a e <$> es
  subst a t (FEConcat e1 e2) = FEConcat (subst a t e1) (subst a t e2)

instance Subst TConName Type FTerm where
  subst _ _ (FEVar x) = FEVar x
  subst c t1 (FEAbs x t2 e) = FEAbs x (subst c t1 t2) (subst c t1 e)
  subst c t (FEApp e1 e2) = FEApp (subst c t e1) (subst c t e2)
  subst c t (FETAbs a e) = FETAbs a (subst c t e)
  subst c t1 (FETApp e t2) = FETApp (subst c t1 e) (subst c t1 t2)
  subst _ _ FETrue = FETrue
  subst _ _ FEFalse = FEFalse
  subst c t (FEIf e1 e2 e3) =
    FEIf (subst c t e1) (subst c t e2) (subst c t e3)
  subst _ _ (FEIntLit i) = FEIntLit i
  subst c t (FEAdd e1 e2) = FEAdd (subst c t e1) (subst c t e2)
  subst c t (FESub e1 e2) = FESub (subst c t e1) (subst c t e2)
  subst c t (FEMul e1 e2) = FEMul (subst c t e1) (subst c t e2)
  subst c t (FEDiv e1 e2) = FEDiv (subst c t e1) (subst c t e2)
  subst c t (FEConcat e1 e2) = FEConcat (subst c t e1) (subst c t e2)
  subst c e (FEList es) = FEList $ subst c e <$> es

instance Subst TVarName Type Type where
  subst a1 t (TVar a2) =
    if a1 == a2
      then t
      else TVar a2
  subst a t (TCon c ts) = TCon c (subst a t <$> ts)
  subst a1 t1 (TForAll a2 t2) =
    TForAll a2 $
    if a1 == a2
      then t2
      else subst a1 t1 t2

instance Subst TConName Type Type where
  subst _ _ (TVar c) = TVar c
  subst c1 t (TCon c2 ts) =
    if c1 == c2
      then t
      else TCon c2 ts
  subst a1 t1 (TForAll a2 t2) = TForAll a2 (subst a1 t1 t2)

-- Collecting binders
newtype BSmallLambda =
  BSmallLambda [(EVarName, Type)]

newtype BBigLambda =
  BBigLambda [TVarName]

newtype BForAll =
  BForAll [TVarName]

class CollectBinders a b where
  collectBinders :: a -> (b, a)

instance CollectBinders ITerm BSmallLambda where
  collectBinders (IEVar x) = (BSmallLambda [], IEVar x)
  collectBinders (IEAbs x t e1) =
    let (BSmallLambda xs, e2) = collectBinders e1
    in (BSmallLambda ((x, t) : xs), e2)
  collectBinders (IEApp e1 e2) = (BSmallLambda [], IEApp e1 e2)
  collectBinders (IELet x e1 e2) = (BSmallLambda [], IELet x e1 e2)
  collectBinders (IEAnno e t) = (BSmallLambda [], IEAnno e t)
  collectBinders IETrue = (BSmallLambda [], IETrue)
  collectBinders IEFalse = (BSmallLambda [], IEFalse)
  collectBinders (IEIf e1 e2 e3) = (BSmallLambda [], IEIf e1 e2 e3)
  collectBinders (IEIntLit i) = (BSmallLambda [], IEIntLit i)
  collectBinders (IEAdd e1 e2) = (BSmallLambda [], IEAdd e1 e2)
  collectBinders (IESub e1 e2) = (BSmallLambda [], IESub e1 e2)
  collectBinders (IEMul e1 e2) = (BSmallLambda [], IEMul e1 e2)
  collectBinders (IEDiv e1 e2) = (BSmallLambda [], IEDiv e1 e2)
  collectBinders (IEList es) = (BSmallLambda [], IEList es)
  collectBinders (IEConcat e1 e2) = (BSmallLambda [], IEConcat e1 e2)

instance CollectBinders FTerm BSmallLambda where
  collectBinders (FEVar x) = (BSmallLambda [], FEVar x)
  collectBinders (FEAbs x t e1) =
    let (BSmallLambda xs, e2) = collectBinders e1
    in (BSmallLambda ((x, t) : xs), e2)
  collectBinders (FEApp e1 e2) = (BSmallLambda [], FEApp e1 e2)
  collectBinders (FETAbs a e) = (BSmallLambda [], FETAbs a e)
  collectBinders (FETApp e t) = (BSmallLambda [], FETApp e t)
  collectBinders FETrue = (BSmallLambda [], FETrue)
  collectBinders FEFalse = (BSmallLambda [], FEFalse)
  collectBinders (FEIf e1 e2 e3) = (BSmallLambda [], FEIf e1 e2 e3)
  collectBinders (FEIntLit i) = (BSmallLambda [], FEIntLit i)
  collectBinders (FEAdd e1 e2) = (BSmallLambda [], FEAdd e1 e2)
  collectBinders (FESub e1 e2) = (BSmallLambda [], FESub e1 e2)
  collectBinders (FEMul e1 e2) = (BSmallLambda [], FEMul e1 e2)
  collectBinders (FEDiv e1 e2) = (BSmallLambda [], FEDiv e1 e2)
  collectBinders (FEList es) = (BSmallLambda [], FEList es)
  collectBinders (FEConcat e1 e2) = (BSmallLambda [], FEConcat e1 e2)

instance CollectBinders FTerm BBigLambda where
  collectBinders (FEVar x) = (BBigLambda [], FEVar x)
  collectBinders (FEAbs x t e) = (BBigLambda [], FEAbs x t e)
  collectBinders (FEApp e1 e2) = (BBigLambda [], FEApp e1 e2)
  collectBinders (FETAbs a e1) =
    let (BBigLambda as, e2) = collectBinders e1
    in (BBigLambda (a : as), e2)
  collectBinders (FETApp e t) = (BBigLambda [], FETApp e t)
  collectBinders FETrue = (BBigLambda [], FETrue)
  collectBinders FEFalse = (BBigLambda [], FEFalse)
  collectBinders (FEIf e1 e2 e3) = (BBigLambda [], FEIf e1 e2 e3)
  collectBinders (FEIntLit i) = (BBigLambda [], FEIntLit i)
  collectBinders (FEAdd e1 e2) = (BBigLambda [], FEAdd e1 e2)
  collectBinders (FESub e1 e2) = (BBigLambda [], FESub e1 e2)
  collectBinders (FEMul e1 e2) = (BBigLambda [], FEMul e1 e2)
  collectBinders (FEDiv e1 e2) = (BBigLambda [], FEDiv e1 e2)
  collectBinders (FEList es) = (BBigLambda [], FEList es)
  collectBinders (FEConcat e1 e2) = (BBigLambda [], FEConcat e1 e2)

instance CollectBinders Type BForAll where
  collectBinders (TVar a) = (BForAll [], TVar a)
  collectBinders (TCon c ts) = (BForAll [], TCon c ts)
  collectBinders (TForAll a t1) =
    let (BForAll as, t2) = collectBinders t1
    in (BForAll (a : as), t2)

instance Show BSmallLambda where
  show (BSmallLambda xs1) =
    "λ" ++
    unwords
      ((\group ->
          "(" ++
          unwords (show . fst <$> group) ++
          " : " ++ show (snd (head group)) ++ ")") <$>
       groupBy (on (==) (show . snd)) xs1)

instance Show BBigLambda where
  show (BBigLambda as) = "Λ" ++ unwords (show <$> as)

instance Show BForAll where
  show (BForAll as) = "∀" ++ unwords (show <$> as)

-- Type annotation propagation
propagate :: ITerm -> Type -> ITerm
propagate (IEAbs x t1 e) t2 =
  let (BForAll _, t3) = collectBinders t2
  in case (t1, t3) of
       (TVar _, TCon c [t4, t5])
         | c == arrowName -> IEAbs x t4 (propagate e t5)
       _ -> IEAbs x t1 e
propagate (IELet x e1 e2) t = IELet x e1 (propagate e2 t)
propagate e _ = e

-- Precedence and associativity of syntactic constructs
data Assoc
  = LeftAssoc
  | RightAssoc
  | NoAssoc
  deriving (Eq)

class Prec a where
  prec :: a -> Integer
  assoc :: a -> Assoc

instance Prec ITerm where
  prec IEVar {} = 10
  prec IEAbs {} = 2
  prec IEApp {} = 10
  prec IELet {} = 2
  prec IEAnno {} = 1
  prec IETrue {} = 10
  prec IEFalse {} = 10
  prec IEIf {} = 2
  prec IEIntLit {} = 10
  prec IEAdd {} = 5
  prec IESub {} = 5
  prec IEMul {} = 6
  prec IEDiv {} = 6
  prec IEList {} = 2
  prec IEConcat {} = 4
  assoc IEVar {} = NoAssoc
  assoc IEAbs {} = RightAssoc
  assoc IEApp {} = LeftAssoc
  assoc IELet {} = RightAssoc
  assoc IEAnno {} = NoAssoc
  assoc IETrue {} = NoAssoc
  assoc IEFalse {} = NoAssoc
  assoc IEIf {} = LeftAssoc
  assoc IEIntLit {} = NoAssoc
  assoc IEAdd {} = LeftAssoc
  assoc IESub {} = LeftAssoc
  assoc IEMul {} = LeftAssoc
  assoc IEDiv {} = LeftAssoc
  assoc IEList {} = NoAssoc
  assoc IEConcat {} = LeftAssoc

instance Prec FTerm where
  prec FEVar {} = 10
  prec FEAbs {} = 2
  prec FEApp {} = 10
  prec FETAbs {} = 2
  prec FETApp {} = 10
  prec FETrue {} = 10
  prec FEFalse {} = 10
  prec FEIf {} = 2
  prec FEIntLit {} = 10
  prec FEAdd {} = 5
  prec FESub {} = 5
  prec FEMul {} = 6
  prec FEDiv {} = 6
  prec FEList {} = 2
  prec FEConcat {} = 4
  assoc FEVar {} = NoAssoc
  assoc FEAbs {} = RightAssoc
  assoc FEApp {} = LeftAssoc
  assoc FETAbs {} = RightAssoc
  assoc FETApp {} = LeftAssoc
  assoc FETrue {} = NoAssoc
  assoc FEFalse {} = NoAssoc
  assoc FEIf {} = RightAssoc
  assoc FEIntLit {} = NoAssoc
  assoc FEAdd {} = LeftAssoc
  assoc FESub {} = LeftAssoc
  assoc FEMul {} = LeftAssoc
  assoc FEDiv {} = LeftAssoc
  assoc FEList {} = NoAssoc
  assoc FEConcat {} = LeftAssoc

instance Prec Type where
  prec TVar {} = 10
  prec (TCon c _)
    | c == arrowName = 1
  prec TCon {} = 10
  prec TForAll {} = 1
  assoc TVar {} = NoAssoc
  assoc (TCon c _)
    | c == arrowName = RightAssoc
  assoc TCon {} = NoAssoc
  assoc TForAll {} = RightAssoc

-- Generate a type variable name that is not in a given set.
freshUserTVarName :: Set TVarName -> TVarName
freshUserTVarName as =
  let names =
        ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ"] ++
        ((++ "′") <$> names)
  in UserTVarName $
     head $ dropWhile (\a -> Set.member (UserTVarName a) as) names

-- This class is provides functionality for renaming all bound type variables
-- such that the minimum number of distinct variables is used and they appear
-- in (Greek) alphabetic order.
class CleanTVars a where
  cleanTVars :: Set TVarName -> a -> a

instance CleanTVars ITerm where
  cleanTVars _ e@(IEVar _) = e
  cleanTVars as (IEAbs x t e) =
    IEAbs
      x
      (cleanTVars (Set.union as (Set.fromList $ freeTVars t)) t)
      (cleanTVars as e)
  cleanTVars as (IEApp e1 e2) = IEApp (cleanTVars as e1) (cleanTVars as e2)
  cleanTVars as (IELet x e1 e2) =
    IELet x (cleanTVars as e1) (cleanTVars as e2)
  cleanTVars as (IEAnno e t) =
    IEAnno
      (cleanTVars as e)
      (cleanTVars (Set.union as (Set.fromList $ freeTVars t)) t)
  cleanTVars _ IETrue = IETrue
  cleanTVars _ IEFalse = IEFalse
  cleanTVars as (IEIf e1 e2 e3) =
    IEIf (cleanTVars as e1) (cleanTVars as e2) (cleanTVars as e3)
  cleanTVars _ e@(IEIntLit _) = e
  cleanTVars as (IEAdd e1 e2) = IEAdd (cleanTVars as e1) (cleanTVars as e2)
  cleanTVars as (IESub e1 e2) = IESub (cleanTVars as e1) (cleanTVars as e2)
  cleanTVars as (IEMul e1 e2) = IEMul (cleanTVars as e1) (cleanTVars as e2)
  cleanTVars as (IEDiv e1 e2) = IEDiv (cleanTVars as e1) (cleanTVars as e2)
  cleanTVars as (IEList es) = IEList $ cleanTVars as <$> es
  cleanTVars as (IEConcat e1 e2) =
    IEConcat (cleanTVars as e1) (cleanTVars as e2)

instance CleanTVars FTerm where
  cleanTVars _ e@(FEVar _) = e
  cleanTVars as (FEAbs x t e) = FEAbs x (cleanTVars as t) (cleanTVars as e)
  cleanTVars as (FEApp e1 e2) = FEApp (cleanTVars as e1) (cleanTVars as e2)
  cleanTVars as (FETAbs a1 e) =
    let a2 = freshUserTVarName as
    in FETAbs a2 (cleanTVars (Set.insert a2 as) (subst a1 (TVar a2) e))
  cleanTVars as (FETApp e t) = FETApp (cleanTVars as e) (cleanTVars as t)
  cleanTVars _ FETrue = FETrue
  cleanTVars _ FEFalse = FEFalse
  cleanTVars as (FEIf e1 e2 e3) =
    FEIf (cleanTVars as e1) (cleanTVars as e2) (cleanTVars as e3)
  cleanTVars _ e@(FEIntLit _) = e
  cleanTVars as (FEAdd e1 e2) = FEAdd (cleanTVars as e1) (cleanTVars as e2)
  cleanTVars as (FESub e1 e2) = FESub (cleanTVars as e1) (cleanTVars as e2)
  cleanTVars as (FEMul e1 e2) = FEMul (cleanTVars as e1) (cleanTVars as e2)
  cleanTVars as (FEDiv e1 e2) = FEDiv (cleanTVars as e1) (cleanTVars as e2)
  cleanTVars as (FEList es) = FEList $ cleanTVars as <$> es
  cleanTVars as (FEConcat e1 e2) =
    FEConcat (cleanTVars as e1) (cleanTVars as e2)

instance CleanTVars Type where
  cleanTVars _ t@(TVar _) = t
  cleanTVars as (TCon c ts) = TCon c (cleanTVars as <$> ts)
  cleanTVars as (TForAll a1 t) =
    let a2 = freshUserTVarName as
    in TForAll a2 (cleanTVars (Set.insert a2 as) (subst a1 (TVar a2) t))

-- Pretty printing
class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint ITerm where
  prettyPrint (IEVar x) = show x
  prettyPrint e1@IEAbs {} =
    let (xs, e2) = collectBinders e1
    in show (xs :: BSmallLambda) ++ " → " ++ embed RightAssoc e1 e2
  prettyPrint e1@(IEApp e2 e3) =
    embed LeftAssoc e1 e2 ++ " " ++ embed RightAssoc e1 e3
  prettyPrint e1@(IELet x e2 e3) =
    show x ++ " = " ++ embed NoAssoc e1 e2 ++ " in " ++ embed RightAssoc e1 e3
  prettyPrint e1@(IEAnno e2 t) =
    embed LeftAssoc e1 e2 ++ " : " ++ prettyPrint t
  prettyPrint IETrue = "true"
  prettyPrint IEFalse = "false"
  prettyPrint e1@(IEIf e2 e3 e4) =
    "if " ++
    embed NoAssoc e1 e2 ++
    " then " ++ embed NoAssoc e1 e3 ++ " else " ++ embed RightAssoc e1 e4
  prettyPrint (IEIntLit i) = show i
  prettyPrint e1@(IEAdd e2 e3) =
    embed LeftAssoc e1 e2 ++ " + " ++ embed RightAssoc e1 e3
  prettyPrint e1@(IESub e2 e3) =
    embed LeftAssoc e1 e2 ++ " - " ++ embed RightAssoc e1 e3
  prettyPrint e1@(IEMul e2 e3) =
    embed LeftAssoc e1 e2 ++ " * " ++ embed RightAssoc e1 e3
  prettyPrint e1@(IEDiv e2 e3) =
    embed LeftAssoc e1 e2 ++ " / " ++ embed RightAssoc e1 e3
  prettyPrint e@(IEList es) =
    "[" ++ intercalate ", " (embed NoAssoc e <$> es) ++ "]"
  prettyPrint e1@(IEConcat e2 e3) =
    embed LeftAssoc e1 e2 ++ " ⧺ " ++ embed RightAssoc e1 e3

instance PrettyPrint FTerm where
  prettyPrint (FEVar x) = show x
  prettyPrint e1@FEAbs {} =
    let (xs, e2) = collectBinders e1
    in show (xs :: BSmallLambda) ++ " → " ++ embed RightAssoc e1 e2
  prettyPrint e1@(FEApp e2 e3) =
    embed LeftAssoc e1 e2 ++ " " ++ embed RightAssoc e1 e3
  prettyPrint e1@(FETAbs _ _) =
    let (as, e2) = collectBinders e1
    in show (as :: BBigLambda) ++ " . " ++ embed RightAssoc e1 e2
  prettyPrint e1@(FETApp e2 t) = embed LeftAssoc e1 e2 ++ " " ++ prettyPrint t
  prettyPrint FETrue = "true"
  prettyPrint FEFalse = "false"
  prettyPrint e1@(FEIf e2 e3 e4) =
    "if " ++
    embed NoAssoc e1 e2 ++
    " then " ++ embed NoAssoc e1 e3 ++ " else " ++ embed RightAssoc e1 e4
  prettyPrint (FEIntLit i) = show i
  prettyPrint e1@(FEAdd e2 e3) =
    embed LeftAssoc e1 e2 ++ " + " ++ embed RightAssoc e1 e3
  prettyPrint e1@(FESub e2 e3) =
    embed LeftAssoc e1 e2 ++ " - " ++ embed RightAssoc e1 e3
  prettyPrint e1@(FEMul e2 e3) =
    embed LeftAssoc e1 e2 ++ " * " ++ embed RightAssoc e1 e3
  prettyPrint e1@(FEDiv e2 e3) =
    embed LeftAssoc e1 e2 ++ " / " ++ embed RightAssoc e1 e3
  prettyPrint e@(FEList es) =
    "[" ++ intercalate ", " (embed NoAssoc e <$> es) ++ "]"
  prettyPrint e1@(FEConcat e2 e3) =
    embed LeftAssoc e1 e2 ++ " ⧺ " ++ embed RightAssoc e1 e3

instance PrettyPrint Type where
  prettyPrint (TVar a) = show a
  prettyPrint t1@(TCon c [t2, t3])
    | c == arrowName =
      embed LeftAssoc t1 t2 ++ " → " ++ embed RightAssoc t1 t3
  prettyPrint (TCon c ts) =
    let params =
          (\t ->
             let s = prettyPrint t
             in if ' ' `elem` s
                  then "(" ++ s ++ ")"
                  else s) <$>
          ts
    in show c ++ (params >>= (" " ++))
  prettyPrint t1@(TForAll _ _) =
    let (as, t2) = collectBinders t1
    in show (as :: BForAll) ++ " . " ++ embed RightAssoc t1 t2

-- The first node is the current one. The second is the one to be embedded.
embed :: (Prec a, PrettyPrint a) => Assoc -> a -> a -> String
embed a e1 e2
  | prec e2 < prec e1 = "(" ++ prettyPrint e2 ++ ")"
  | prec e2 == prec e1 && a == LeftAssoc && assoc e2 == RightAssoc =
    "(" ++ prettyPrint e2 ++ ")"
  | prec e2 == prec e1 && a == RightAssoc && assoc e2 == LeftAssoc =
    "(" ++ prettyPrint e2 ++ ")"
  | otherwise = prettyPrint e2

instance Show ITerm where
  show = prettyPrint . cleanTVars Set.empty

instance Show FTerm where
  show e = prettyPrint $ cleanTVars (Set.fromList $ freeTVars e) e

instance Show Type where
  show t = prettyPrint $ cleanTVars (Set.fromList $ freeTVars t) t
