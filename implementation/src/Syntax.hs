module Syntax
  ( Term(..)
  , Type(..) ) where

data Term -- Metavariable: e
  = EVar String
  | EAbs String Term
  | EApp Term Term
  | EAnno Term Type
  deriving Eq

instance Show Term where
  show (EVar x) = x
  show (EAbs x e) = "\\" ++ x ++ " . " ++ show e
  show (EApp (EAbs x e1) (EApp e2 e3)) =
    "(" ++ show (EAbs x e1) ++ ") (" ++ show (EApp e2 e3) ++ ")"
  show (EApp (EAbs x e1) e2) = "(" ++ show (EAbs x e1) ++ ") " ++ show e2
  show (EApp (EAnno e1 t) (EApp e2 e3)) =
    "(" ++ show (EAnno e1 t) ++ ") (" ++ show (EApp e2 e3) ++ ")"
  show (EApp (EAnno e1 t) e2) = "(" ++ show (EAnno e1 t) ++ ") " ++ show e2
  show (EApp e1 (EApp e2 e3)) = show e1 ++ " (" ++ show (EApp e2 e3) ++ ")"
  show (EApp e1 e2) = show e1 ++ " " ++ show e2
  show (EAnno e t) = show e ++ " : " ++ show t

data Type -- Metavariable: t
  = TVar String
  | TArrow Type Type
  | TForAll String Type
  deriving Eq

instance Show Type where
  show (TVar x) = x
  show (TArrow (TVar x) t) = x ++ " -> " ++ show t
  show (TArrow t1 t2) = "(" ++ show t1 ++ ") -> " ++ show t2
  show (TForAll x t) = "forall " ++ x ++ " . " ++ show t
