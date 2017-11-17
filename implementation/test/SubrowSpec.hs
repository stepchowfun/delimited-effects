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

occurs :: Effect -> Row Effect -> Bool
occurs _ REmpty = False
occurs z1 (RSingleton z2) = z1 == z2
occurs z (RUnion r1 r2) = occurs z r1 || occurs z r2

allOccur :: Row Effect -> Bool
allOccur r = all (\z -> occurs z r) effects

-- The QuickCheck specs

specContainedImpliesSubrow :: Row Effect -> Row Effect -> Bool
specContainedImpliesSubrow r1 r2 =
  allOccur (RUnion r1 r2) ||
  not (contained r1 r2) ||
  subrow r1 r2

specSubrowImpliesContained :: Row Effect -> Row Effect -> Bool
specSubrowImpliesContained r1 r2 =
  allOccur (RUnion r1 r2) ||
  not (subrow r1 r2) ||
  contained r1 r2

subrowSpec :: Spec
subrowSpec = modifyMaxSuccess (const 100000) $ describe "subrow" $ do
  it "returns True for r1 <= r2 if r2 contains all the effects in r1" $ do
    property specContainedImpliesSubrow
  it "returns True for r1 <= r2 implies r2 contains all the effects in r1" $ do
    property specSubrowImpliesContained
