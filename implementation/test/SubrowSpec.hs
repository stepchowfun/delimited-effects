module SubrowSpec (subrowSpec) where

import Lib (Row(..), TypeVar, typeVars, subrow)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (property)

-- Helper functions

rowContains :: TypeVar -> Row -> Bool
rowContains _  REmpty          = False
rowContains x1 (RSingleton x2) = x1 == x2
rowContains x  (RUnion r1 r2 ) = rowContains x r1 || rowContains x r2

contained :: Row -> Row -> Bool
contained r1 r2 =
  all (\a -> not (rowContains a r1) || rowContains a r2) typeVars

-- The QuickCheck specs

specContainedIffSubrow :: Row -> Row -> Bool
specContainedIffSubrow r1 r2 = contained r1 r2 == subrow r1 r2

subrowSpec :: Spec
subrowSpec =
  modifyMaxSuccess (const 100000)
    $ describe "subrow"
    $ it "returns True for r1 <= r2 iff r2 contains all the effects in r1"
    $ property specContainedIffSubrow
