module Subtype (subtype) where

import Subrow (subrow)
import Syntax (Type(..))

subtype :: Ord a => Type a -> Type a -> Bool
subtype TUnit TUnit = True
subtype TUnit _ = False
subtype (TArrow t1 t2 r1) (TArrow t3 t4 r2) =
     subtype t3 t1
  && subtype t2 t4
  && subrow r1 r2
subtype (TArrow _ _ _) _ = False
