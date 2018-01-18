module Subrow
  ( subrow
  ) where

import Syntax (Row(..), rowContains)

subrow :: Row -> Row -> Bool
subrow REmpty _ = True
subrow (RSingleton x) r = rowContains x r
subrow (RUnion r1 r2) r3 = subrow r1 r3 && subrow r2 r3
