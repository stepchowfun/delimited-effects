module SubrowSpec (subrowSpec) where

import Lib (Row(..), typeVars, rowContains, subrow)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (property)

-- Helper functions

contained :: Row -> Row -> Bool
contained r1 r2 =
  all (\a -> not (rowContains a r1) || rowContains a r2) typeVars

-- The QuickCheck specs

specContainedIffSubrow :: Row -> Row -> Bool
specContainedIffSubrow r1 r2 = contained r1 r2 == subrow r1 r2

subrowSpec :: Spec
subrowSpec = modifyMaxSuccess (const 100000) $ describe "subrow" $ do
  it "returns True for r1 <= r2 iff r2 contains all the effects in r1" $ do
    property specContainedIffSubrow
