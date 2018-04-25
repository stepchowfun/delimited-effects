{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Syntax
  ( FTerm(..)
  , Term(..)
  , Type(..)
  ) where

import Data.Function (on)
import Data.List (groupBy)

data Term -- Metavariable: e
  = EVar String
  | EAbs String
         Term
  | EApp Term
         Term
  | EAnno Term
          Type
  deriving (Eq)

data FTerm -- Metavariable: e
  = FEVar String
  | FEAbs String
          Type
          FTerm
  | FEApp FTerm
          FTerm
  | FETAbs String
           FTerm
  | FETApp FTerm
           Type
  deriving (Eq)

data Type -- Metavariable: t
  = TVar String
  | TArrow Type
           Type
  | TForAll String
            Type
  deriving (Eq)

class CollectParams a b | a -> b where
  collectParams :: a -> ([b], a)

instance CollectParams Term String where
  collectParams (EVar x) = ([], EVar x)
  collectParams (EAbs x e1) =
    let (xs, e2) = collectParams e1
    in (x : xs, e2)
  collectParams (EApp e1 e2) = ([], EApp e1 e2)
  collectParams (EAnno e t) = ([], EAnno e t)

instance CollectParams FTerm (String, String) where
  collectParams (FEVar x) = ([], FEVar x)
  collectParams (FEAbs x t e1) =
    let (xs, e2) = collectParams e1
    in ((x, show t) : xs, e2)
  collectParams (FEApp e1 e2) = ([], FEApp e1 e2)
  collectParams (FETAbs x e1) =
    let (xs, e2) = collectParams e1
    in ((x, "*") : xs, e2)
  collectParams (FETApp e t) = ([], FETApp e t)

instance CollectParams Type String where
  collectParams (TVar x) = ([], TVar x)
  collectParams (TArrow t1 t2) = ([], TArrow t1 t2)
  collectParams (TForAll x t1) =
    let (xs, t2) = collectParams t1
    in (x : xs, t2)

class PresentParams a where
  presentParams :: [a] -> String

instance PresentParams String where
  presentParams = unwords

instance PresentParams (String, String) where
  presentParams xs =
    unwords $ do
      group <- groupBy (on (==) snd) xs
      let ys = fst <$> group
      let t = snd (head group)
      return $ "(" ++ unwords ys ++ " : " ++ t ++ ")"

instance Show Term where
  show (EVar x) = x
  show (EAbs x e1) =
    let (xs, e2) = collectParams (EAbs x e1)
    in "λ" ++ presentParams xs ++ " . " ++ show e2
  show (EApp (EAbs x e1) (EApp e2 e3)) =
    "(" ++ show (EAbs x e1) ++ ") (" ++ show (EApp e2 e3) ++ ")"
  show (EApp (EAbs x e1) e2) = "(" ++ show (EAbs x e1) ++ ") " ++ show e2
  show (EApp (EAnno e1 t) (EApp e2 e3)) =
    "(" ++ show (EAnno e1 t) ++ ") (" ++ show (EApp e2 e3) ++ ")"
  show (EApp (EAnno e1 t) e2) = "(" ++ show (EAnno e1 t) ++ ") " ++ show e2
  show (EApp e1 (EApp e2 e3)) = show e1 ++ " (" ++ show (EApp e2 e3) ++ ")"
  show (EApp e1 e2) = show e1 ++ " " ++ show e2
  show (EAnno e t) = show e ++ " : " ++ show t

instance Show FTerm where
  show (FEVar x) = x
  show (FEAbs x t e1) =
    let (xs, e2) = collectParams (FEAbs x t e1)
    in "λ" ++ presentParams xs ++ " . " ++ show e2
  show (FEApp (FEAbs x t e1) (FEApp e2 e3)) =
    "(" ++ show (FEAbs x t e1) ++ ") (" ++ show (FEApp e2 e3) ++ ")"
  show (FEApp (FEAbs x t1 e1) (FETApp e2 t2)) =
    "(" ++ show (FEAbs x t1 e1) ++ ") (" ++ show (FETApp e2 t2) ++ ")"
  show (FEApp (FEAbs x t e1) e2) =
    "(" ++ show (FEAbs x t e1) ++ ") " ++ show e2
  show (FEApp (FETAbs x e1) (FEApp e2 e3)) =
    "(" ++ show (FETAbs x e1) ++ ") (" ++ show (FEApp e2 e3) ++ ")"
  show (FEApp (FETAbs x e1) (FETApp e2 t)) =
    "(" ++ show (FETAbs x e1) ++ ") (" ++ show (FETApp e2 t) ++ ")"
  show (FEApp (FETAbs x e1) e2) = "(" ++ show (FETAbs x e1) ++ ") " ++ show e2
  show (FEApp e1 (FEApp e2 e3)) = show e1 ++ " (" ++ show (FEApp e2 e3) ++ ")"
  show (FEApp e1 (FETApp e2 t)) = show e1 ++ " (" ++ show (FETApp e2 t) ++ ")"
  show (FEApp e1 e2) = show e1 ++ " " ++ show e2
  show (FETAbs x e1) =
    let (xs, e2) = collectParams (FETAbs x e1)
    in "λ" ++ presentParams xs ++ " . " ++ show e2
  show (FETApp (FEAbs x t1 e) t2) =
    "(" ++ show (FEAbs x t1 e) ++ ") " ++ show t2
  show (FETApp (FETAbs x e) t) = "(" ++ show (FETAbs x e) ++ ") " ++ show t
  show (FETApp e t) = show e ++ " " ++ show t

instance Show Type where
  show (TVar x) = x
  show (TArrow (TVar x) t) = x ++ " -> " ++ show t
  show (TArrow t1 t2) = "(" ++ show t1 ++ ") -> " ++ show t2
  show (TForAll x t1) =
    let (xs, t2) = collectParams (TForAll x t1)
    in "∀" ++ presentParams xs ++ " . " ++ show t2
