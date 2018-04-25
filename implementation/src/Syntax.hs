module Syntax
  ( Term(..)
  , Type(..) ) where

data Term -- Metavariable: e
  = EVar String
  | EAbs String Term
  | EApp Term Term
  | EAnno Term Type
  deriving Eq

data Type -- Metavariable: t
  = TVar String
  | TArrow Type Type
  | TForAll String Type
  deriving Eq

collectAbs :: Term -> ([String], Term)
collectAbs (EVar x     ) = ([], EVar x)
collectAbs (EAbs  x  e1) = let (xs, e2) = collectAbs e1 in (x : xs, e2)
collectAbs (EApp  e1 e2) = ([], EApp e1 e2)
collectAbs (EAnno e  t ) = ([], EAnno e t)

collectForAll :: Type -> ([String], Type)
collectForAll (TVar x       ) = ([], TVar x)
collectForAll (TArrow  t1 t2) = ([], TArrow t1 t2)
collectForAll (TForAll x  t1) = let (xs, t2) = collectForAll t1 in (x : xs, t2)

instance Show Term where
  show (EVar x) = x
  show (EAbs x e1) =
    let (xs, e2) = collectAbs (EAbs x e1)
    in  "\\" ++ unwords xs ++ " . " ++ show e2
  show (EApp (EAbs x e1) (EApp e2 e3)) =
    "(" ++ show (EAbs x e1) ++ ") (" ++ show (EApp e2 e3) ++ ")"
  show (EApp (EAbs x e1) e2) =
    "(" ++ show (EAbs x e1) ++ ") " ++ show e2
  show (EApp (EAnno e1 t) (EApp e2 e3)) =
    "(" ++ show (EAnno e1 t) ++ ") (" ++ show (EApp e2 e3) ++ ")"
  show (EApp (EAnno e1 t) e2) =
    "(" ++ show (EAnno e1 t) ++ ") " ++ show e2
  show (EApp e1 (EApp e2 e3)) =
    show e1 ++ " (" ++ show (EApp e2 e3) ++ ")"
  show (EApp  e1 e2) = show e1 ++ " " ++ show e2
  show (EAnno e  t ) = show e ++ " : " ++ show t

instance Show Type where
  show (TVar x) = x
  show (TArrow (TVar x) t) = x ++ " -> " ++ show t
  show (TArrow t1 t2) = "(" ++ show t1 ++ ") -> " ++ show t2
  show (TForAll x t1) =
    let (xs, t2) = collectForAll (TForAll x t1)
    in  "forall " ++ unwords xs ++ " . " ++ show t2
