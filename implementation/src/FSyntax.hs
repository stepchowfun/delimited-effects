module FSyntax
  ( FTerm(..)
  , FType(..) ) where

data FTerm -- Metavariable: e
  = FEVar String
  | FEAbs String FType FTerm
  | FEApp FTerm FTerm
  | FETAbs String FTerm
  | FETApp FTerm FType
  deriving Eq

instance Show FTerm where
  show (FEVar x) = x
  show (FEAbs x t e) = "\\" ++ x ++ " : " ++ show t ++ " . " ++ show e
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
  show (FETAbs x e) = "\\" ++ x ++ " . " ++ show e
  show (FETApp (FEAbs x t1 e) t2) =
    "(" ++ show (FEAbs x t1 e) ++ ") " ++ show t2
  show (FETApp (FETAbs x e) t) = "(" ++ show (FETAbs x e) ++ ") " ++ show t
  show (FETApp e t) = show e ++ " " ++ show t

data FType -- Metavariable: t
  = FTVar String
  | FTArrow FType FType
  | FTForAll String FType
  deriving Eq

instance Show FType where
  show (FTVar x) = x
  show (FTArrow (FTVar x) t) = x ++ " -> " ++ show t
  show (FTArrow t1 t2) = "(" ++ show t1 ++ ") -> " ++ show t2
  show (FTForAll x t) = "forall " ++ x ++ " . " ++ show t
