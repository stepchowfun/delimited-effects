module Subrow (subrow) where

import Syntax (Row(..), rowContains)

subrow :: Eq a => Row a -> Row a -> Bool
subrow REmpty _ = True
subrow (RSingleton x) r = rowContains x r
subrow (RUnion r1 r2) r3 = subrow r1 r3 && subrow r2 r3
