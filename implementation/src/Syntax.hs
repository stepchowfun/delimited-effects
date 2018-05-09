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
  , collectBinders
  , freeEVars
  , freeTConsts
  , freeTVars
  , subst
  ) where

import Data.Function (on)
import Data.List (groupBy, nub)

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
  = IEIntLit Integer
  | IEAddInt ITerm
             ITerm
  | IESubInt ITerm
             ITerm
  | IEMulInt ITerm
             ITerm
  | IEDivInt ITerm
             ITerm
  | IEVar EVarName
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

data FTerm
  = FEIntLit Integer
  | FEAddInt FTerm
             FTerm
  | FESubInt FTerm
             FTerm
  | FEMulInt FTerm
             FTerm
  | FEDivInt FTerm
             FTerm
  | FEVar EVarName
  | FEAbs EVarName
          Type
          FTerm
  | FEApp FTerm
          FTerm
  | FETAbs TVarName
           FTerm
  | FETApp FTerm
           Type

data Type
  = TVar TVarName
  | TConst TConstName
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
  freeEVars (IEIntLit _) = []
  freeEVars (IEAddInt e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (IESubInt e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (IEMulInt e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (IEDivInt e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (IEVar x) = [x]
  freeEVars (IEAbs x _ e) = filter (/= x) (freeEVars e)
  freeEVars (IEApp e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (IELet x e1 e2) =
    nub $ freeEVars e1 ++ filter (/= x) (freeEVars e2)
  freeEVars (IEAnno e _) = freeEVars e

instance FreeTVars ITerm where
  freeTVars (IEIntLit _) = []
  freeTVars (IEAddInt e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (IESubInt e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (IEMulInt e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (IEDivInt e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (IEVar _) = []
  freeTVars (IEAbs _ Nothing e) = nub $ freeTVars e
  freeTVars (IEAbs _ (Just t) e) = nub $ freeTVars t ++ freeTVars e
  freeTVars (IEApp e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (IELet _ e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (IEAnno e t) = nub $ freeTVars e ++ freeTVars t

instance FreeTConsts ITerm where
  freeTConsts (IEIntLit _) = []
  freeTConsts (IEAddInt e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (IESubInt e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (IEMulInt e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (IEDivInt e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (IEVar _) = []
  freeTConsts (IEAbs _ Nothing e) = nub $ freeTConsts e
  freeTConsts (IEAbs _ (Just t) e) = nub $ freeTConsts t ++ freeTConsts e
  freeTConsts (IEApp e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (IELet _ e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (IEAnno e t) = nub $ freeTConsts e ++ freeTConsts t

instance FreeEVars FTerm where
  freeEVars (FEIntLit _) = []
  freeEVars (FEAddInt e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (FESubInt e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (FEMulInt e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (FEDivInt e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (FEVar x) = [x]
  freeEVars (FEAbs x _ e) = filter (/= x) (freeEVars e)
  freeEVars (FEApp e1 e2) = nub $ freeEVars e1 ++ freeEVars e2
  freeEVars (FETAbs _ e) = freeEVars e
  freeEVars (FETApp e _) = freeEVars e

instance FreeTVars FTerm where
  freeTVars (FEIntLit _) = []
  freeTVars (FEAddInt e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (FESubInt e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (FEMulInt e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (FEDivInt e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (FEVar _) = []
  freeTVars (FEAbs _ t e) = nub $ freeTVars t ++ freeTVars e
  freeTVars (FEApp e1 e2) = nub $ freeTVars e1 ++ freeTVars e2
  freeTVars (FETAbs a e) = filter (/= a) (freeTVars e)
  freeTVars (FETApp e t) = nub $ freeTVars e ++ freeTVars t

instance FreeTConsts FTerm where
  freeTConsts (FEIntLit _) = []
  freeTConsts (FEAddInt e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (FESubInt e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (FEMulInt e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (FEDivInt e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (FEVar _) = []
  freeTConsts (FEAbs _ t e) = nub $ freeTConsts t ++ freeTConsts e
  freeTConsts (FEApp e1 e2) = nub $ freeTConsts e1 ++ freeTConsts e2
  freeTConsts (FETAbs _ e) = freeTConsts e
  freeTConsts (FETApp e t) = nub $ freeTConsts e ++ freeTConsts t

instance FreeTVars Type where
  freeTVars (TVar a) = [a]
  freeTVars (TConst _) = []
  freeTVars (TArrow t1 t2) = nub $ freeTVars t1 ++ freeTVars t2
  freeTVars (TForAll a t) = filter (/= a) (freeTVars t)

instance FreeTConsts Type where
  freeTConsts (TVar _) = []
  freeTConsts (TConst c) = [c]
  freeTConsts (TArrow t1 t2) = nub $ freeTConsts t1 ++ freeTConsts t2
  freeTConsts (TForAll _ t) = freeTConsts t

-- Substitution
class Subst a b c where
  subst :: a -> b -> c -> c

instance Subst EVarName ITerm ITerm where
  subst _ _ (IEIntLit i) = IEIntLit i
  subst x e1 (IEAddInt e2 e3) = IEAddInt (subst x e1 e2) (subst x e1 e3)
  subst x e1 (IESubInt e2 e3) = IESubInt (subst x e1 e2) (subst x e1 e3)
  subst x e1 (IEMulInt e2 e3) = IEMulInt (subst x e1 e2) (subst x e1 e3)
  subst x e1 (IEDivInt e2 e3) = IEDivInt (subst x e1 e2) (subst x e1 e3)
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

instance Subst TVarName Type ITerm where
  subst _ _ (IEIntLit i) = IEIntLit i
  subst a t (IEAddInt e1 e2) = IEAddInt (subst a t e1) (subst a t e2)
  subst a t (IESubInt e1 e2) = IESubInt (subst a t e1) (subst a t e2)
  subst a t (IEMulInt e1 e2) = IEMulInt (subst a t e1) (subst a t e2)
  subst a t (IEDivInt e1 e2) = IEDivInt (subst a t e1) (subst a t e2)
  subst _ _ (IEVar x) = IEVar x
  subst a t (IEAbs x Nothing e) = IEAbs x Nothing (subst a t e)
  subst a t1 (IEAbs x (Just t2) e) =
    IEAbs x (Just $ subst a t1 t2) (subst a t1 e)
  subst a t (IEApp e1 e2) = IEApp (subst a t e1) (subst a t e2)
  subst a t (IELet x e1 e2) = IELet x (subst a t e1) (subst a t e2)
  subst a t1 (IEAnno e t2) = IEAnno (subst a t1 e) (subst a t1 t2)

instance Subst TConstName Type ITerm where
  subst _ _ (IEIntLit i) = IEIntLit i
  subst c t (IEAddInt e1 e2) = IEAddInt (subst c t e1) (subst c t e2)
  subst c t (IESubInt e1 e2) = IESubInt (subst c t e1) (subst c t e2)
  subst c t (IEMulInt e1 e2) = IEMulInt (subst c t e1) (subst c t e2)
  subst c t (IEDivInt e1 e2) = IEDivInt (subst c t e1) (subst c t e2)
  subst _ _ (IEVar x) = IEVar x
  subst c t (IEAbs x Nothing e) = IEAbs x Nothing (subst c t e)
  subst c t1 (IEAbs x (Just t2) e) =
    IEAbs x (Just $ subst c t1 t2) (subst c t1 e)
  subst c t (IEApp e1 e2) = IEApp (subst c t e1) (subst c t e2)
  subst c t (IELet x e1 e2) = IELet x (subst c t e1) (subst c t e2)
  subst c t1 (IEAnno e t2) = IEAnno (subst c t1 e) (subst c t1 t2)

instance Subst EVarName FTerm FTerm where
  subst _ _ (FEIntLit i) = FEIntLit i
  subst x e1 (FEAddInt e2 e3) = FEAddInt (subst x e1 e2) (subst x e1 e3)
  subst x e1 (FESubInt e2 e3) = FESubInt (subst x e1 e2) (subst x e1 e3)
  subst x e1 (FEMulInt e2 e3) = FEMulInt (subst x e1 e2) (subst x e1 e3)
  subst x e1 (FEDivInt e2 e3) = FEDivInt (subst x e1 e2) (subst x e1 e3)
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

instance Subst TVarName Type FTerm where
  subst _ _ (FEIntLit i) = FEIntLit i
  subst a t (FEAddInt e1 e2) = FEAddInt (subst a t e1) (subst a t e2)
  subst a t (FESubInt e1 e2) = FESubInt (subst a t e1) (subst a t e2)
  subst a t (FEMulInt e1 e2) = FEMulInt (subst a t e1) (subst a t e2)
  subst a t (FEDivInt e1 e2) = FEDivInt (subst a t e1) (subst a t e2)
  subst _ _ (FEVar x) = FEVar x
  subst a t1 (FEAbs x t2 e) = FEAbs x (subst a t1 t2) (subst a t1 e)
  subst a t (FEApp e1 e2) = FEApp (subst a t e1) (subst a t e2)
  subst a1 t (FETAbs a2 e) = FETAbs a2 (subst a1 t e)
  subst a t1 (FETApp e t2) = FETApp (subst a t1 e) (subst a t1 t2)

instance Subst TConstName Type FTerm where
  subst _ _ (FEIntLit i) = FEIntLit i
  subst c t (FEAddInt e1 e2) = FEAddInt (subst c t e1) (subst c t e2)
  subst c t (FESubInt e1 e2) = FESubInt (subst c t e1) (subst c t e2)
  subst c t (FEMulInt e1 e2) = FEMulInt (subst c t e1) (subst c t e2)
  subst c t (FEDivInt e1 e2) = FEDivInt (subst c t e1) (subst c t e2)
  subst _ _ (FEVar x) = FEVar x
  subst c t1 (FEAbs x t2 e) = FEAbs x (subst c t1 t2) (subst c t1 e)
  subst c t (FEApp e1 e2) = FEApp (subst c t e1) (subst c t e2)
  subst c t (FETAbs a e) = FETAbs a (subst c t e)
  subst c t1 (FETApp e t2) = FETApp (subst c t1 e) (subst c t1 t2)

instance Subst TVarName Type Type where
  subst a1 t (TVar a2) =
    if a1 == a2
      then t
      else TVar a2
  subst _ _ (TConst c) = TConst c
  subst a t1 (TArrow t2 t3) = TArrow (subst a t1 t2) (subst a t1 t3)
  subst a1 t1 (TForAll a2 t2) =
    TForAll a2 $
    if a1 == a2
      then t2
      else subst a1 t1 t2

instance Subst TConstName Type Type where
  subst _ _ (TVar c) = TVar c
  subst c1 t (TConst c2) =
    if c1 == c2
      then t
      else TConst c2
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
  collectBinders (IEIntLit i) = (BSmallLambda [], IEIntLit i)
  collectBinders (IEAddInt e1 e2) = (BSmallLambda [], IEAddInt e1 e2)
  collectBinders (IESubInt e1 e2) = (BSmallLambda [], IESubInt e1 e2)
  collectBinders (IEMulInt e1 e2) = (BSmallLambda [], IEMulInt e1 e2)
  collectBinders (IEDivInt e1 e2) = (BSmallLambda [], IEDivInt e1 e2)
  collectBinders (IEVar x) = (BSmallLambda [], IEVar x)
  collectBinders (IEAbs x t e1) =
    let (BSmallLambda xs, e2) = collectBinders e1
    in (BSmallLambda ((x, t) : xs), e2)
  collectBinders (IEApp e1 e2) = (BSmallLambda [], IEApp e1 e2)
  collectBinders (IELet x e1 e2) = (BSmallLambda [], IELet x e1 e2)
  collectBinders (IEAnno e t) = (BSmallLambda [], IEAnno e t)

instance CollectBinders FTerm BSmallLambda where
  collectBinders (FEIntLit i) = (BSmallLambda [], FEIntLit i)
  collectBinders (FEAddInt e1 e2) = (BSmallLambda [], FEAddInt e1 e2)
  collectBinders (FESubInt e1 e2) = (BSmallLambda [], FESubInt e1 e2)
  collectBinders (FEMulInt e1 e2) = (BSmallLambda [], FEMulInt e1 e2)
  collectBinders (FEDivInt e1 e2) = (BSmallLambda [], FEDivInt e1 e2)
  collectBinders (FEVar x) = (BSmallLambda [], FEVar x)
  collectBinders (FEAbs x t e1) =
    let (BSmallLambda xs, e2) = collectBinders e1
    in (BSmallLambda ((x, Just t) : xs), e2)
  collectBinders (FEApp e1 e2) = (BSmallLambda [], FEApp e1 e2)
  collectBinders (FETAbs a e) = (BSmallLambda [], FETAbs a e)
  collectBinders (FETApp e t) = (BSmallLambda [], FETApp e t)

instance CollectBinders FTerm BBigLambda where
  collectBinders (FEIntLit i) = (BBigLambda [], FEIntLit i)
  collectBinders (FEAddInt e1 e2) = (BBigLambda [], FEAddInt e1 e2)
  collectBinders (FESubInt e1 e2) = (BBigLambda [], FESubInt e1 e2)
  collectBinders (FEMulInt e1 e2) = (BBigLambda [], FEMulInt e1 e2)
  collectBinders (FEDivInt e1 e2) = (BBigLambda [], FEDivInt e1 e2)
  collectBinders (FEVar x) = (BBigLambda [], FEVar x)
  collectBinders (FEAbs x t e) = (BBigLambda [], FEAbs x t e)
  collectBinders (FEApp e1 e2) = (BBigLambda [], FEApp e1 e2)
  collectBinders (FETAbs a e1) =
    let (BBigLambda as, e2) = collectBinders e1
    in (BBigLambda (a : as), e2)
  collectBinders (FETApp e t) = (BBigLambda [], FETApp e t)

instance CollectBinders Type BForAll where
  collectBinders (TVar a) = (BForAll [], TVar a)
  collectBinders (TConst c) = (BForAll [], TConst c)
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

instance Show ITerm where
  show (IEIntLit i) = show i
  show (IEAddInt e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (IESubInt e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
  show (IEMulInt e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (IEDivInt e1 e2) = "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
  show (IEVar x) = show x
  show (IEAbs x t e1) =
    let (xs, e2) = collectBinders (IEAbs x t e1)
    in "(" ++ show (xs :: BSmallLambda) ++ " → " ++ show e2 ++ ")"
  show (IEApp e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (IELet x e1 e2) =
    "(" ++ show x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"
  show (IEAnno e t) = "(" ++ show e ++ " : " ++ show t ++ ")"

instance Show FTerm where
  show (FEIntLit i) = show i
  show (FEAddInt e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (FESubInt e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
  show (FEMulInt e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (FEDivInt e1 e2) = "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
  show (FEVar x) = show x
  show (FEAbs x t e1) =
    let (xs, e2) = collectBinders (FEAbs x t e1)
    in "(" ++ show (xs :: BSmallLambda) ++ " → " ++ show e2 ++ ")"
  show (FEApp e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (FETAbs a e1) =
    let (as, e2) = collectBinders (FETAbs a e1)
    in "(" ++ show (as :: BBigLambda) ++ " → " ++ show e2 ++ ")"
  show (FETApp e t) = "(" ++ show e ++ " " ++ show t ++ ")"

instance Show Type where
  show (TVar a) = show a
  show (TConst c) = show c
  show (TArrow (TVar a) t) = show a ++ " → " ++ show t
  show (TArrow (TConst c) t) = show c ++ " → " ++ show t
  show (TArrow t1 t2) = "(" ++ show t1 ++ ") → " ++ show t2
  show (TForAll a t1) =
    let (as, t2) = collectBinders (TForAll a t1)
    in show (as :: BForAll) ++ " . " ++ show t2
