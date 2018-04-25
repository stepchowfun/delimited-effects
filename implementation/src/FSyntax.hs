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

data FType -- Metavariable: t
  = FTVar String
  | FTArrow FType FType
  | FTForAll String FType
  deriving Eq

collectAbs :: FTerm -> ([String], FTerm)
collectAbs (FEVar x) = ([], FEVar x)
collectAbs (FEAbs x t e1) =
  let (xs, e2) = collectAbs e1
  in  (("(" ++ x ++ " : " ++ show t ++ ")") : xs, e2)
collectAbs (FEApp e1 e2) = ([], FEApp e1 e2)
collectAbs (FETAbs x e1) =
  let (xs, e2) = collectAbs e1 in (("(" ++ x ++ " : *)") : xs, e2)
collectAbs (FETApp e t) = ([], FETApp e t)

collectForAll :: FType -> ([String], FType)
collectForAll (FTVar x      ) = ([], FTVar x)
collectForAll (FTArrow t1 t2) = ([], FTArrow t1 t2)
collectForAll (FTForAll x t1) =
  let (xs, t2) = collectForAll t1 in (x : xs, t2)

instance Show FTerm where
  show (FEVar x) = x
  show (FEAbs x t e1) =
    let (xs, e2) = collectAbs (FEAbs x t e1)
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
    let (xs, e2) = collectAbs (FETAbs x e1)
    in  "\\" ++ unwords xs ++ " . " ++ show e2
  show (FETApp (FEAbs x t1 e) t2) =
    "(" ++ show (FEAbs x t1 e) ++ ") " ++ show t2
  show (FETApp (FETAbs x e) t) =
    "(" ++ show (FETAbs x e) ++ ") " ++ show t
  show (FETApp e t) = show e ++ " " ++ show t

instance Show FType where
  show (FTVar x) = x
  show (FTArrow (FTVar x) t) = x ++ " -> " ++ show t
  show (FTArrow t1 t2) = "(" ++ show t1 ++ ") -> " ++ show t2
  show (FTForAll x t1) =
    let (xs, t2) = collectForAll (FTForAll x t1)
    in  "forall " ++ unwords xs ++ " . " ++ show t2
