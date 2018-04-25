module Syntax
  ( FTerm(..)
  , Term(..)
  , Type(..) ) where

data Term -- Metavariable: e
  = EVar String
  | EAbs String Term
  | EApp Term Term
  | EAnno Term Type
  deriving Eq

data FTerm -- Metavariable: e
  = FEVar String
  | FEAbs String Type FTerm
  | FEApp FTerm FTerm
  | FETAbs String FTerm
  | FETApp FTerm Type
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

fCollectAbs :: FTerm -> ([String], FTerm)
fCollectAbs (FEVar x) = ([], FEVar x)
fCollectAbs (FEAbs x t e1) =
  let (xs, e2) = fCollectAbs e1
  in  (("(" ++ x ++ " : " ++ show t ++ ")") : xs, e2)
fCollectAbs (FEApp e1 e2) = ([], FEApp e1 e2)
fCollectAbs (FETAbs x e1) =
  let (xs, e2) = fCollectAbs e1 in (("(" ++ x ++ " : *)") : xs, e2)
fCollectAbs (FETApp e t) = ([], FETApp e t)

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

instance Show FTerm where
  show (FEVar x) = x
  show (FEAbs x t e1) =
    let (xs, e2) = fCollectAbs (FEAbs x t e1)
    in  "\\" ++ unwords xs ++ " . " ++ show e2
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
  show (FEApp (FETAbs x e1) e2) =
    "(" ++ show (FETAbs x e1) ++ ") " ++ show e2
  show (FEApp e1 (FEApp e2 e3)) =
    show e1 ++ " (" ++ show (FEApp e2 e3) ++ ")"
  show (FEApp e1 (FETApp e2 t)) =
    show e1 ++ " (" ++ show (FETApp e2 t) ++ ")"
  show (FEApp e1 e2) = show e1 ++ " " ++ show e2
  show (FETAbs x e1) =
    let (xs, e2) = fCollectAbs (FETAbs x e1)
    in  "\\" ++ unwords xs ++ " . " ++ show e2
  show (FETApp (FEAbs x t1 e) t2) =
    "(" ++ show (FEAbs x t1 e) ++ ") " ++ show t2
  show (FETApp (FETAbs x e) t) =
    "(" ++ show (FETAbs x e) ++ ") " ++ show t
  show (FETApp e t) = show e ++ " " ++ show t

instance Show Type where
  show (TVar x) = x
  show (TArrow (TVar x) t) = x ++ " -> " ++ show t
  show (TArrow t1 t2) = "(" ++ show t1 ++ ") -> " ++ show t2
  show (TForAll x t1) =
    let (xs, t2) = collectForAll (TForAll x t1)
    in  "forall " ++ unwords xs ++ " . " ++ show t2
