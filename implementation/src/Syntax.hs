{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Syntax
  ( BBigLambda(..)
  , BForAll(..)
  , BSmallLambda(..)
  , EVarName(..)
  , FTerm(..)
  , FreeEVars
  , FreeTConsts
  , FreeTVars
  , ITerm(..)
  , Subst
  , TConstName(..)
  , TVarName(..)
  , Type(..)
  , annotate
  , boolName
  , boolType
  , collectBinders
  , freeEVars
  , freeTConsts
  , freeTVars
  , intName
  , intType
  , listName
  , listType
  , subst
  ) where

import Data.Function (on)
import Data.List (groupBy, intercalate, nub, unwords)

-- Data types
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

data TConstName
  = UserTConstName String -- Can be referred to in source programs
  | AutoTConstName Integer -- Cannot be referred to in source programs
  deriving (Eq, Ord)

instance Show TConstName where
  show (UserTConstName s) = s
  show (AutoTConstName i) = "$" ++ show i

data ITerm
  = IEVar EVarName
  | IEAbs EVarName
          (Maybe Type)
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
  | FEAddInt FTerm
             FTerm
  | FESubInt FTerm
             FTerm
  | FEMulInt FTerm
             FTerm
  | FEDivInt FTerm
             FTerm
  | FEList [FTerm]
  | FEConcat FTerm
             FTerm

data Type
  = TVar TVarName
  | TConst TConstName
           [Type]
  | TArrow Type
           Type
  | TForAll TVarName
            Type

-- Free variables
class FreeEVars a where
  freeEVars :: a -> [EVarName]

class FreeTVars a where
  freeTVars :: a -> [TVarName]

class FreeTConsts a where
  freeTConsts :: a -> [TConstName]

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

instance FreeTVars ITerm where
  freeTVars (IEVar _) = []
  freeTVars (IEAbs _ Nothing e) = nub $ freeTVars e
  freeTVars (IEAbs _ (Just t) e) = nub $ freeTVars t ++ freeTVars e
  freeTVars (IEApp e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (IELet _ e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (IEAnno e t) = nub $ freeTVars e ++ freeTVars t
  freeTVars IETrue = []
  freeTVars IEFalse = []
  freeTVars (IEIf e1 e2 e3) =
    nub $ freeTVars e1 ++ freeTVars e2 ++ freeTVars e3
  freeTVars (IEIntLit _) = []
  freeTVars (IEAdd e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (IESub e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (IEMul e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (IEDiv e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (IEList es) = nub $ es >>= freeTVars
  freeTVars (IEConcat e1 e2) = nub $ freeTVars e1 ++ freeTVars e2

instance FreeTConsts ITerm where
  freeTConsts (IEVar _) = []
  freeTConsts (IEAbs _ Nothing e) = nub $ freeTConsts e
  freeTConsts (IEAbs _ (Just t) e) = nub $ freeTConsts t ++ freeTConsts e
  freeTConsts (IEApp e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (IELet _ e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (IEAnno e t) = nub $ freeTConsts e ++ freeTConsts t
  freeTConsts IETrue = []
  freeTConsts IEFalse = []
  freeTConsts (IEIf e1 e2 e3) =
    nub $ freeTConsts e1 ++ freeTConsts e2 ++ freeTConsts e3
  freeTConsts (IEIntLit _) = []
  freeTConsts (IEAdd e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (IESub e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (IEMul e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (IEDiv e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (IEList es) = nub $ es >>= freeTConsts
  freeTConsts (IEConcat e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2

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
  freeEVars (FEAddInt e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (FESubInt e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (FEMulInt e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (FEDivInt e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
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
  freeTVars (FEAddInt e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (FESubInt e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (FEMulInt e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (FEDivInt e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (FEList es) = nub $ es >>= freeTVars
  freeTVars (FEConcat e1 e2) = nub $ freeTVars e1 ++ freeTVars e2

instance FreeTConsts FTerm where
  freeTConsts (FEVar _) = []
  freeTConsts (FEAbs _ t e) = nub $ freeTConsts t ++ freeTConsts e
  freeTConsts (FEApp e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (FETAbs _ e) = freeTConsts e
  freeTConsts (FETApp e t) = nub $ freeTConsts e ++ freeTConsts t
  freeTConsts FETrue = []
  freeTConsts FEFalse = []
  freeTConsts (FEIf e1 e2 e3) =
    nub $ freeTConsts e1 ++ freeTConsts e2 ++ freeTConsts e3
  freeTConsts (FEIntLit _) = []
  freeTConsts (FEAddInt e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (FESubInt e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (FEMulInt e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (FEDivInt e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (FEConcat e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (FEList es) = nub $ es >>= freeTConsts

instance FreeTVars Type where
  freeTVars (TVar a) = [a]
  freeTVars (TConst _ ts) = foldr (\t as -> freeTVars t ++ as) [] ts
  freeTVars (TArrow t1 t2) = nub $ freeTVars t1 ++ freeTVars t2
  freeTVars (TForAll a t) = filter (/= a) (freeTVars t)

instance FreeTConsts Type where
  freeTConsts (TVar _) = []
  freeTConsts (TConst c ts) = c : foldr (\t cs -> freeTConsts t ++ cs) [] ts
  freeTConsts (TArrow t1 t2) = nub $ freeTConsts t1 ++ freeTConsts t2
  freeTConsts (TForAll _ t) = freeTConsts t

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

instance Subst TVarName Type ITerm where
  subst _ _ (IEVar x) = IEVar x
  subst a t (IEAbs x Nothing e) = IEAbs x Nothing (subst a t e)
  subst a t1 (IEAbs x (Just t2) e) =
    IEAbs x (Just $ subst a t1 t2) (subst a t1 e)
  subst a t (IEApp e1 e2) = IEApp (subst a t e1) (subst a t e2)
  subst a t (IELet x e1 e2) = IELet x (subst a t e1) (subst a t e2)
  subst a t1 (IEAnno e t2) = IEAnno (subst a t1 e) (subst a t1 t2)
  subst _ _ IETrue = IETrue
  subst _ _ IEFalse = IEFalse
  subst a t (IEIf e1 e2 e3) =
    IEIf (subst a t e1) (subst a t e2) (subst a t e3)
  subst _ _ (IEIntLit i) = IEIntLit i
  subst a t (IEAdd e1 e2) = IEAdd (subst a t e1) (subst a t e2)
  subst a t (IESub e1 e2) = IESub (subst a t e1) (subst a t e2)
  subst a t (IEMul e1 e2) = IEMul (subst a t e1) (subst a t e2)
  subst a t (IEDiv e1 e2) = IEDiv (subst a t e1) (subst a t e2)
  subst a e (IEList es) = IEList $ subst a e <$> es
  subst a t (IEConcat e1 e2) = IEConcat (subst a t e1) (subst a t e2)

instance Subst TConstName Type ITerm where
  subst _ _ (IEVar x) = IEVar x
  subst c t (IEAbs x Nothing e) = IEAbs x Nothing (subst c t e)
  subst c t1 (IEAbs x (Just t2) e) =
    IEAbs x (Just $ subst c t1 t2) (subst c t1 e)
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
  subst x e1 (FEAddInt e2 e3) = FEAddInt (subst x e1 e2) (subst x e1 e3)
  subst x e1 (FESubInt e2 e3) = FESubInt (subst x e1 e2) (subst x e1 e3)
  subst x e1 (FEMulInt e2 e3) = FEMulInt (subst x e1 e2) (subst x e1 e3)
  subst x e1 (FEDivInt e2 e3) = FEDivInt (subst x e1 e2) (subst x e1 e3)
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
  subst a t (FEAddInt e1 e2) = FEAddInt (subst a t e1) (subst a t e2)
  subst a t (FESubInt e1 e2) = FESubInt (subst a t e1) (subst a t e2)
  subst a t (FEMulInt e1 e2) = FEMulInt (subst a t e1) (subst a t e2)
  subst a t (FEDivInt e1 e2) = FEDivInt (subst a t e1) (subst a t e2)
  subst a e (FEList es) = FEList $ subst a e <$> es
  subst a t (FEConcat e1 e2) = FEConcat (subst a t e1) (subst a t e2)

instance Subst TConstName Type FTerm where
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
  subst c t (FEAddInt e1 e2) = FEAddInt (subst c t e1) (subst c t e2)
  subst c t (FESubInt e1 e2) = FESubInt (subst c t e1) (subst c t e2)
  subst c t (FEMulInt e1 e2) = FEMulInt (subst c t e1) (subst c t e2)
  subst c t (FEDivInt e1 e2) = FEDivInt (subst c t e1) (subst c t e2)
  subst c t (FEConcat e1 e2) = FEConcat (subst c t e1) (subst c t e2)
  subst c e (FEList es) = FEList $ subst c e <$> es

instance Subst TVarName Type Type where
  subst a1 t (TVar a2) =
    if a1 == a2
      then t
      else TVar a2
  subst a t (TConst c ts) = TConst c (subst a t <$> ts)
  subst a t1 (TArrow t2 t3) = TArrow (subst a t1 t2) (subst a t1 t3)
  subst a1 t1 (TForAll a2 t2) =
    TForAll a2 $
    if a1 == a2
      then t2
      else subst a1 t1 t2

instance Subst TConstName Type Type where
  subst _ _ (TVar c) = TVar c
  subst c1 t (TConst c2 ts) =
    if c1 == c2
      then t
      else TConst c2 ts
  subst a t1 (TArrow t2 t3) = TArrow (subst a t1 t2) (subst a t1 t3)
  subst a1 t1 (TForAll a2 t2) = TForAll a2 (subst a1 t1 t2)

-- Collecting binders
newtype BSmallLambda =
  BSmallLambda [(EVarName, Maybe Type)]

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
    in (BSmallLambda ((x, Just t) : xs), e2)
  collectBinders (FEApp e1 e2) = (BSmallLambda [], FEApp e1 e2)
  collectBinders (FETAbs a e) = (BSmallLambda [], FETAbs a e)
  collectBinders (FETApp e t) = (BSmallLambda [], FETApp e t)
  collectBinders FETrue = (BSmallLambda [], FETrue)
  collectBinders FEFalse = (BSmallLambda [], FEFalse)
  collectBinders (FEIf e1 e2 e3) = (BSmallLambda [], FEIf e1 e2 e3)
  collectBinders (FEIntLit i) = (BSmallLambda [], FEIntLit i)
  collectBinders (FEAddInt e1 e2) = (BSmallLambda [], FEAddInt e1 e2)
  collectBinders (FESubInt e1 e2) = (BSmallLambda [], FESubInt e1 e2)
  collectBinders (FEMulInt e1 e2) = (BSmallLambda [], FEMulInt e1 e2)
  collectBinders (FEDivInt e1 e2) = (BSmallLambda [], FEDivInt e1 e2)
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
  collectBinders (FEAddInt e1 e2) = (BBigLambda [], FEAddInt e1 e2)
  collectBinders (FESubInt e1 e2) = (BBigLambda [], FESubInt e1 e2)
  collectBinders (FEMulInt e1 e2) = (BBigLambda [], FEMulInt e1 e2)
  collectBinders (FEDivInt e1 e2) = (BBigLambda [], FEDivInt e1 e2)
  collectBinders (FEList es) = (BBigLambda [], FEList es)
  collectBinders (FEConcat e1 e2) = (BBigLambda [], FEConcat e1 e2)

instance CollectBinders Type BForAll where
  collectBinders (TVar a) = (BForAll [], TVar a)
  collectBinders (TConst c ts) = (BForAll [], TConst c ts)
  collectBinders (TArrow t1 t2) = (BForAll [], TArrow t1 t2)
  collectBinders (TForAll a t1) =
    let (BForAll as, t2) = collectBinders t1
    in (BForAll (a : as), t2)

-- Type annotation propagation
annotate :: ITerm -> Type -> ITerm
annotate (IEAbs x t1 e) t2 =
  let (BForAll _, t3) = collectBinders t2
  in case (t1, t3) of
       (Nothing, TArrow t4 t5) -> IEAbs x (Just t4) (annotate e t5)
       (Just t4, TArrow _ t5) -> IEAbs x (Just t4) (annotate e t5)
       _ -> IEAbs x t1 e
annotate (IELet x e1 e2) t = IELet x e1 (annotate e2 t)
annotate e _ = e

-- Pretty printing
instance Show BSmallLambda where
  show (BSmallLambda xs1) =
    "λ" ++
    unwords
      ((\group ->
          let t1 = snd (head group)
          in "(" ++
             unwords (show . fst <$> group) ++
             " : " ++
             (case t1 of
                Nothing -> "?"
                Just t2 -> show t2) ++
             ")") <$>
       groupBy (on (==) (show . snd)) xs1)

instance Show BBigLambda where
  show (BBigLambda as) = "Λ" ++ unwords (show <$> as)

instance Show BForAll where
  show (BForAll as) = "∀" ++ unwords (show <$> as)

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
  prec FEAddInt {} = 5
  prec FESubInt {} = 5
  prec FEMulInt {} = 6
  prec FEDivInt {} = 6
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
  assoc FEAddInt {} = LeftAssoc
  assoc FESubInt {} = LeftAssoc
  assoc FEMulInt {} = LeftAssoc
  assoc FEDivInt {} = LeftAssoc
  assoc FEList {} = NoAssoc
  assoc FEConcat {} = LeftAssoc

instance Prec Type where
  prec TVar {} = 10
  prec TConst {} = 10
  prec TArrow {} = 1
  prec TForAll {} = 1
  assoc TVar {} = NoAssoc
  assoc TConst {} = NoAssoc
  assoc TArrow {} = RightAssoc
  assoc TForAll {} = RightAssoc

-- The first node is the current one. The second is the one to be embedded.
embed :: (Prec a, Show a) => Assoc -> a -> a -> String
embed a e1 e2
  | prec e2 < prec e1 = "(" ++ show e2 ++ ")"
  | prec e2 == prec e1 && a == LeftAssoc && assoc e2 == RightAssoc =
    "(" ++ show e2 ++ ")"
  | prec e2 == prec e1 && a == RightAssoc && assoc e2 == LeftAssoc =
    "(" ++ show e2 ++ ")"
  | otherwise = show e2

instance Show ITerm where
  show (IEVar x) = show x
  show e1@(IEAbs x t e2) =
    let (xs, e3) = collectBinders (IEAbs x t e2)
    in show (xs :: BSmallLambda) ++ " → " ++ embed RightAssoc e1 e3
  show e1@(IEApp e2 e3) =
    embed LeftAssoc e1 e2 ++ " " ++ embed RightAssoc e1 e3
  show e1@(IELet x e2 e3) =
    show x ++ " = " ++ embed NoAssoc e1 e2 ++ " in " ++ embed RightAssoc e1 e3
  show e1@(IEAnno e2 t) = embed LeftAssoc e1 e2 ++ " : " ++ show t
  show IETrue = "true"
  show IEFalse = "false"
  show e1@(IEIf e2 e3 e4) =
    "if " ++
    embed NoAssoc e1 e2 ++
    " then " ++ embed NoAssoc e1 e3 ++ " else " ++ embed RightAssoc e1 e4
  show (IEIntLit i) = show i
  show e1@(IEAdd e2 e3) =
    embed LeftAssoc e1 e2 ++ " + " ++ embed RightAssoc e1 e3
  show e1@(IESub e2 e3) =
    embed LeftAssoc e1 e2 ++ " - " ++ embed RightAssoc e1 e3
  show e1@(IEMul e2 e3) =
    embed LeftAssoc e1 e2 ++ " * " ++ embed RightAssoc e1 e3
  show e1@(IEDiv e2 e3) =
    embed LeftAssoc e1 e2 ++ " / " ++ embed RightAssoc e1 e3
  show e@(IEList es) = "[" ++ intercalate ", " (embed NoAssoc e <$> es) ++ "]"
  show e1@(IEConcat e2 e3) =
    embed LeftAssoc e1 e2 ++ " ⧺ " ++ embed RightAssoc e1 e3

instance Show FTerm where
  show (FEVar x) = show x
  show e1@(FEAbs x t e2) =
    let (xs, e3) = collectBinders (FEAbs x t e2)
    in show (xs :: BSmallLambda) ++ " → " ++ embed RightAssoc e1 e3
  show e1@(FEApp e2 e3) =
    embed LeftAssoc e1 e2 ++ " " ++ embed RightAssoc e1 e3
  show e1@(FETAbs a e2) =
    let (as, e3) = collectBinders (FETAbs a e2)
    in show (as :: BBigLambda) ++ " . " ++ embed RightAssoc e1 e3
  show e1@(FETApp e2 t) = embed LeftAssoc e1 e2 ++ " " ++ show t
  show FETrue = "true"
  show FEFalse = "false"
  show e1@(FEIf e2 e3 e4) =
    "if " ++
    embed NoAssoc e1 e2 ++
    " then " ++ embed NoAssoc e1 e3 ++ " else " ++ embed RightAssoc e1 e4
  show (FEIntLit i) = show i
  show e1@(FEAddInt e2 e3) =
    embed LeftAssoc e1 e2 ++ " + " ++ embed RightAssoc e1 e3
  show e1@(FESubInt e2 e3) =
    embed LeftAssoc e1 e2 ++ " - " ++ embed RightAssoc e1 e3
  show e1@(FEMulInt e2 e3) =
    embed LeftAssoc e1 e2 ++ " * " ++ embed RightAssoc e1 e3
  show e1@(FEDivInt e2 e3) =
    embed LeftAssoc e1 e2 ++ " / " ++ embed RightAssoc e1 e3
  show e@(FEList es) = "[" ++ intercalate ", " (embed NoAssoc e <$> es) ++ "]"
  show e1@(FEConcat e2 e3) =
    embed LeftAssoc e1 e2 ++ " ⧺ " ++ embed RightAssoc e1 e3

instance Show Type where
  show (TVar a) = show a
  show (TConst c ts) =
    let params =
          unwords
            ((\t ->
                let s = show t
                in if ' ' `elem` s && (c /= listName || length ts > 1)
                     then "(" ++ s ++ ")"
                     else s) <$>
             ts)
    in if c == listName
         then "[" ++ params ++ "]"
         else show c ++
              (if null params
                 then ""
                 else " " ++ params)
  show t1@(TArrow t2 t3) =
    embed LeftAssoc t1 t2 ++ " → " ++ embed RightAssoc t1 t3
  show t1@(TForAll a t2) =
    let (as, t3) = collectBinders (TForAll a t2)
    in show (as :: BForAll) ++ " . " ++ embed RightAssoc t1 t3

-- Built-in type constants
boolName :: TConstName
boolName = UserTConstName "Bool"

boolType :: Type
boolType = TConst boolName []

intName :: TConstName
intName = UserTConstName "Int"

intType :: Type
intType = TConst intName []

listName :: TConstName
listName = UserTConstName "List"

listType :: Type -> Type
listType t = TConst listName [t]
