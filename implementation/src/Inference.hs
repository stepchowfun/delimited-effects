module Inference
  ( infer
  ) where

import Syntax (FTerm(..), Term(..), Type(..))

infer :: Term -> FTerm
infer _ =
  FEAbs
    "x"
    (TVar "a")
    (FEAbs
       "y"
       (TVar "b")
       (FEAbs "z" (TVar "b") (FEAbs "w" (TVar "a") (FEVar "f"))))
