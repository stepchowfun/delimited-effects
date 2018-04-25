module Inference
  ( infer ) where

import FSyntax (FTerm(..))
import Syntax (Term(..))

infer :: Term -> FTerm
infer _ = FEVar "x"
