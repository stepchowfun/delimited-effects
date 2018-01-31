module Subrow (subrow) where

import Syntax (Row(..))

subrow :: Row -> Row -> Bool
subrow REmpty          _               = True
subrow (RSingleton _ ) REmpty          = False
subrow (RSingleton x1) (RSingleton x2) = x1 == x2
subrow (RSingleton x) (RUnion r1 r2) =
  subrow (RSingleton x) r1 || subrow (RSingleton x) r2
subrow (RUnion r1 r2) r3 = subrow r1 r3 && subrow r2 r3
