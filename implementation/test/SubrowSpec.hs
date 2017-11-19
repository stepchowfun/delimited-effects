module SubrowSpec (subrowSpec) where

import Lib (Row(..), rowContains, subrow)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (property)
import Types (Effect(..), effects)

-- Helper functions

contained :: Row Effect -> Row Effect -> Bool
contained r1 r2 =
  all (\z -> not (rowContains z r1) || rowContains z r2) effects

-- The QuickCheck specs

specContainedIffSubrow :: Row Effect -> Row Effect -> Bool
specContainedIffSubrow r1 r2 = contained r1 r2 == subrow r1 r2

subrowSpec :: Spec
subrowSpec = modifyMaxSuccess (const 100000) $ describe "subrow" $ do
  it "returns True for r1 <= r2 iff r2 contains all the effects in r1" $ do
    property specContainedIffSubrow
