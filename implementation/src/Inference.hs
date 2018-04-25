module Inference
  ( infer ) where

import Syntax (FTerm(..), Term(..), Type(..))

infer :: Term -> FTerm
infer _ = FEAbs "x"
                (TForAll "a" $ TForAll "b" (TArrow (TVar "a") (TVar "b")))
                (FEVar "x")
