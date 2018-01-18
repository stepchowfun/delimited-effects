module SubrowSpec (subrowSpec) where

import Lib (Row(..), effects, rowContains, subrow)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (property)

-- Helper functions

contained :: Row -> Row -> Bool
contained r1 r2 =
  all (\z -> not (rowContains z r1) || rowContains z r2) effects

-- The QuickCheck specs

specContainedIffSubrow :: Row -> Row -> Bool
specContainedIffSubrow r1 r2 = contained r1 r2 == subrow r1 r2

subrowSpec :: Spec
subrowSpec = modifyMaxSuccess (const 100000) $ describe "subrow" $ do
  it "returns True for r1 <= r2 iff r2 contains all the effects in r1" $ do
    property specContainedIffSubrow
