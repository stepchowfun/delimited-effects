{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Syntax
  ( CollectParams
  , EVar(..)
  , FTerm(..)
  , FreeEVars
  , FreeTVars
  , ITerm(..)
  , PresentParams
  , Subst
  , TVar(..)
  , Type(..)
  , collectParams
  , freeEVars
  , freeTVars
  , presentParams
  , subst
  ) where

import Data.Function (on)
import Data.List (groupBy)

-- Data types
data EVar
  = UserEVar String
  | FreshEVar String
  deriving (Eq, Ord)

instance Show EVar where
  show (UserEVar s) = s
  show (FreshEVar s) = s

data TVar
  = UserTVar String
  | FreshTVar String
  deriving (Eq, Ord)

instance Show TVar where
  show (UserTVar s) = s
  show (FreshTVar s) = s

data ITerm
  = IEIntLit Integer
  | IEAddInt ITerm
             ITerm
  | IEVar EVar
  | IEAbs EVar
          (Maybe Type)
          ITerm
  | IEApp ITerm
          ITerm
  | IELet EVar
          ITerm
          ITerm
  | IEAnno ITerm
           Type

data FTerm
  = FEIntLit Integer
  | FEAddInt FTerm
             FTerm
  | FEVar EVar
  | FEAbs EVar
          Type
          FTerm
  | FEApp FTerm
          FTerm
  | FETAbs TVar
           FTerm
  | FETApp FTerm
           Type

data Type
  = TVar TVar
  | TArrow Type
           Type
  | TForAll TVar
            Type

-- Free variables
class FreeEVars a where
  freeEVars :: a -> [EVar]

class FreeTVars a where
  freeTVars :: a -> [TVar]

instance FreeEVars ITerm where
  freeEVars (IEIntLit _) = []
  freeEVars (IEAddInt e1 e2) = freeEVars e1 ++ freeEVars e2
  freeEVars (IEVar x) = [x]
  freeEVars (IEAbs x _ e) = filter (/= x) (freeEVars e)
  freeEVars (IEApp e1 e2) = freeEVars e1 ++ freeEVars e2
  freeEVars (IELet x e1 e2) = freeEVars e1 ++ filter (/= x) (freeEVars e2)
  freeEVars (IEAnno e _) = freeEVars e

instance FreeTVars ITerm where
  freeTVars (IEIntLit _) = []
  freeTVars (IEAddInt e1 e2) = freeTVars e1 ++ freeTVars e2
  freeTVars (IEVar _) = []
  freeTVars (IEAbs _ (Just t) e) = freeTVars t ++ freeTVars e
  freeTVars (IEAbs _ Nothing e) = freeTVars e
  freeTVars (IEApp e1 e2) = freeTVars e1 ++ freeTVars e2
  freeTVars (IELet _ e1 e2) = freeTVars e1 ++ freeTVars e2
  freeTVars (IEAnno _ t) = freeTVars t

instance FreeEVars FTerm where
  freeEVars (FEIntLit _) = []
  freeEVars (FEAddInt e1 e2) = freeEVars e1 ++ freeEVars e2
  freeEVars (FEVar x) = [x]
  freeEVars (FEAbs x _ e) = filter (/= x) (freeEVars e)
  freeEVars (FEApp e1 e2) = freeEVars e1 ++ freeEVars e2
  freeEVars (FETAbs _ e) = freeEVars e
  freeEVars (FETApp e _) = freeEVars e

instance FreeTVars FTerm where
  freeTVars (FEIntLit _) = []
  freeTVars (FEAddInt e1 e2) = freeTVars e1 ++ freeTVars e2
  freeTVars (FEVar _) = []
  freeTVars (FEAbs _ t e) = freeTVars t ++ freeTVars e
  freeTVars (FEApp e1 e2) = freeTVars e1 ++ freeTVars e2
  freeTVars (FETAbs a e) = filter (/= a) (freeTVars e)
  freeTVars (FETApp e t) = freeTVars e ++ freeTVars t

instance FreeTVars Type where
  freeTVars (TVar a) = [a]
  freeTVars (TArrow t1 t2) = freeTVars t1 ++ freeTVars t2
  freeTVars (TForAll a t) = filter (/= a) (freeTVars t)

-- Substitution
class Subst a b c where
  subst :: a -> b -> c -> c

instance Subst EVar ITerm ITerm where
  subst _ _ (IEIntLit i) = IEIntLit i
  subst x e1 (IEAddInt e2 e3) = IEAddInt (subst x e1 e2) (subst x e1 e3)
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

instance Subst TVar Type ITerm where
  subst _ _ (IEIntLit i) = IEIntLit i
  subst a t (IEAddInt e1 e2) = IEAddInt (subst a t e1) (subst a t e2)
  subst _ _ (IEVar x) = IEVar x
  subst a t1 (IEAbs x t2 e2) = IEAbs x (subst a t1 <$> t2) (subst a t1 e2)
  subst a t (IEApp e1 e2) = IEApp (subst a t e1) (subst a t e2)
  subst a t (IELet x e1 e2) = IELet x (subst a t e1) (subst a t e2)
  subst a t1 (IEAnno e t2) = IEAnno (subst a t1 e) (subst a t1 t2)

instance Subst EVar FTerm FTerm where
  subst _ _ (FEIntLit i) = FEIntLit i
  subst x e1 (FEAddInt e2 e3) = FEAddInt (subst x e1 e2) (subst x e1 e3)
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

instance Subst TVar Type FTerm where
  subst _ _ (FEIntLit i) = FEIntLit i
  subst a t (FEAddInt e1 e2) = FEAddInt (subst a t e1) (subst a t e2)
  subst _ _ (FEVar x) = FEVar x
  subst a t1 (FEAbs x t2 e) = FEAbs x (subst a t1 t2) (subst a t1 e)
  subst a t (FEApp e1 e2) = FEApp (subst a t e1) (subst a t e2)
  subst a1 t (FETAbs a2 e) = FETAbs a2 (subst a1 t e)
  subst a t1 (FETApp e t2) = FETApp (subst a t2 e) (subst a t1 t2)

instance Subst TVar Type Type where
  subst a1 t (TVar a2) =
    if a1 == a2
      then t
      else TVar a2
  subst a t1 (TArrow t2 t3) = TArrow (subst a t1 t2) (subst a t1 t3)
  subst a1 t1 (TForAll a2 t2) =
    TForAll a2 $
    if a1 == a2
      then t2
      else subst a1 t1 t2

-- Equality
instance Eq ITerm where
  IEIntLit i1 == IEIntLit i2 = i1 == i2
  IEAddInt e1 e2 == IEAddInt e3 e4 = e1 == e3 && e2 == e4
  IEVar x1 == IEVar x2 = x1 == x2
  IEAbs x1 t1 e1 == IEAbs x2 t2 e2 =
    t1 == t2 && e1 == subst x2 (IEVar x1) e2 && e2 == subst x1 (IEVar x2) e1
  IEApp e1 e2 == IEApp e3 e4 = e1 == e3 && e2 == e4
  IEAnno e1 t1 == IEAnno e2 t2 = e1 == e2 && t1 == t2
  _ == _ = False

instance Eq FTerm where
  FEIntLit i1 == FEIntLit i2 = i1 == i2
  FEAddInt e1 e2 == FEAddInt e3 e4 = e1 == e3 && e2 == e4
  FEVar x1 == FEVar x2 = x1 == x2
  FEAbs x1 t1 e1 == FEAbs x2 t2 e2 =
    e1 == subst x2 (FEVar x1) e2 && e2 == subst x1 (FEVar x2) e1 && t1 == t2
  FEApp e1 e2 == FEApp e3 e4 = e1 == e3 && e2 == e4
  FETAbs a1 e1 == FETAbs a2 e2 =
    e1 == subst a2 (TVar a1) e2 && e2 == subst a1 (TVar a2) e1
  FETApp e1 t1 == FETApp e2 t2 = e1 == e2 && t1 == t2
  _ == _ = False

instance Eq Type where
  TVar a1 == TVar a2 = a1 == a2
  TArrow t1 t2 == TArrow t3 t4 = t1 == t3 && t2 == t4
  TForAll a1 t1 == TForAll a2 t2 =
    t1 == subst a2 (TVar a1) t2 && t2 == subst a1 (TVar a2) t1
  _ == _ = False

-- Pretty printing
class CollectParams a b | a -> b where
  collectParams :: a -> ([b], a)

instance CollectParams ITerm (String, Maybe String) where
  collectParams (IEIntLit i) = ([], IEIntLit i)
  collectParams (IEAddInt e1 e2) = ([], IEAddInt e1 e2)
  collectParams (IEVar x) = ([], IEVar x)
  collectParams (IEAbs x Nothing e1) =
    let (xs, e2) = collectParams e1
    in ((show x, Nothing) : xs, e2)
  collectParams (IEAbs x (Just t) e1) =
    let (xs, e2) = collectParams e1
    in ((show x, Just $ show t) : xs, e2)
  collectParams (IEApp e1 e2) = ([], IEApp e1 e2)
  collectParams (IELet x e1 e2) = ([], IELet x e1 e2)
  collectParams (IEAnno e t) = ([], IEAnno e t)

instance CollectParams FTerm (String, String) where
  collectParams (FEIntLit i) = ([], FEIntLit i)
  collectParams (FEAddInt e1 e2) = ([], FEAddInt e1 e2)
  collectParams (FEVar x) = ([], FEVar x)
  collectParams (FEAbs x t e1) =
    let (xs, e2) = collectParams e1
    in ((show x, show t) : xs, e2)
  collectParams (FEApp e1 e2) = ([], FEApp e1 e2)
  collectParams (FETAbs a e1) =
    let (xs, e2) = collectParams e1
    in ((show a, "*") : xs, e2)
  collectParams (FETApp e t) = ([], FETApp e t)

instance CollectParams Type String where
  collectParams (TVar a) = ([], TVar a)
  collectParams (TArrow t1 t2) = ([], TArrow t1 t2)
  collectParams (TForAll a t1) =
    let (as, t2) = collectParams t1
    in (show a : as, t2)

class PresentParams a where
  presentParams :: [a] -> String

instance PresentParams String where
  presentParams = unwords

instance PresentParams (String, Maybe String) where
  presentParams xs =
    unwords $ do
      group <- groupBy (on (==) snd) xs
      let ys = fst <$> group
      case snd (head group) of
        Just t -> return $ "(" ++ unwords ys ++ " : " ++ t ++ ")"
        Nothing -> return $ unwords ys

instance PresentParams (String, String) where
  presentParams xs =
    unwords $ do
      group <- groupBy (on (==) snd) xs
      let ys = fst <$> group
      let t = snd (head group)
      return $ "(" ++ unwords ys ++ " : " ++ t ++ ")"

instance Show ITerm where
  show (IEIntLit i) = show i
  show (IEAddInt e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (IEVar x) = show x
  show (IEAbs x t e1) =
    let (xs, e2) = collectParams (IEAbs x t e1)
    in "(λ" ++ presentParams xs ++ " . " ++ show e2 ++ ")"
  show (IEApp e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (IELet x e1 e2) =
    "(let " ++ show x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"
  show (IEAnno e t) = "(" ++ show e ++ " : " ++ show t ++ ")"

instance Show FTerm where
  show (FEIntLit i) = show i
  show (FEAddInt e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (FEVar x) = show x
  show (FEAbs x t e1) =
    let (xs, e2) = collectParams (FEAbs x t e1)
    in "(λ" ++ presentParams xs ++ " . " ++ show e2 ++ ")"
  show (FEApp e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (FETAbs a e1) =
    let (as, e2) = collectParams (FETAbs a e1)
    in "(λ" ++ presentParams as ++ " . " ++ show e2 ++ ")"
  show (FETApp e t) = "(" ++ show e ++ " " ++ show t ++ ")"

instance Show Type where
  show (TVar a) = show a
  show (TArrow (TVar a) t) = show a ++ " -> " ++ show t
  show (TArrow t1 t2) = "(" ++ show t1 ++ ") -> " ++ show t2
  show (TForAll a t1) =
    let (as, t2) = collectParams (TForAll a t1)
    in "∀" ++ presentParams as ++ " . " ++ show t2
