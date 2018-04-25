module Inference
  ( infer ) where

import FSyntax (FTerm(..), FType(..))
import Syntax (Term(..))

infer :: Term -> FTerm
infer _ = FEAbs
  "x"
  (FTForAll "a" $ FTForAll "b" (FTArrow (FTVar "a") (FTVar "b")))
  (FEVar "x")
