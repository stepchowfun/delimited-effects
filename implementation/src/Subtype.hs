module Subtype (subtype) where

import Subrow (subrow)
import Syntax (Row(..), Type(..))

subtype :: Ord a => Type a -> Row a -> Type a -> Row a -> Bool
subtype TUnit r1 TUnit r2 = subrow r1 r2
subtype TUnit _ _ _ = False
subtype (TArrow t1 t2 r1) r2 (TArrow t3 t4 r3) r4 =
     subtype t3 REmpty t1 REmpty
  && subtype t2 r1 t4 r3
  && subrow r2 r4
subtype (TArrow _ _ _) _ _ _ = False
