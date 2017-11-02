module SubrowSpec (subrowSpec) where

import Data.List (foldl')
import Data.Stream (Stream(..), head, tail)
import Lib (Row(..), subrow)
import Test.Hspec (Spec, describe, it, pending)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
  ( Arbitrary
  , arbitrary
  , elements
  , oneof
  , property
  , shrink
  , suchThat )

-- Types

data Effect = EffectA | EffectB | EffectC | EffectD | EffectE
  deriving (Eq, Ord, Show)

effects = [EffectA, EffectB, EffectC, EffectD, EffectE]

-- Arbitrary instances

instance Arbitrary a => Arbitrary (Row a) where
  arbitrary = oneof
    [ pure REmpty
    , RSingleton <$> arbitrary
    , RUnion <$> arbitrary <*> arbitrary
    , RDifference <$> arbitrary <*> arbitrary
    ]
  shrink REmpty = []
  shrink (RSingleton _) = []
  shrink (RUnion x y) =
    [x, y] ++ [RUnion x' y' | (x', y') <- shrink (x, y)]
  shrink (RDifference x y) =
    [x] ++ [RDifference x' y' | (x', y') <- shrink (x, y)]

instance Arbitrary Effect where
  arbitrary = elements effects

-- Helper functions

contains :: Effect -> Row Effect -> Bool
contains e REmpty = False
contains e (RSingleton x) = x == e
contains e (RUnion x y) = contains e x || contains e y
contains e (RDifference x y) = contains e x && not (contains e y)

contained :: Row Effect -> Row Effect -> Bool
contained x y = all (\e -> not (contains e x) || contains e y) effects

occurs :: Effect -> Row Effect -> Bool
occurs e REmpty = False
occurs e (RSingleton x) = x == e
occurs e (RUnion x y) = occurs e x || occurs e y
occurs e (RDifference x y) = occurs e x || occurs e y

allOccur :: Row Effect -> Bool
allOccur x = all (\e -> occurs e x) effects

-- The QuickCheck specs

specContainedImpliesSubrow :: Row Effect -> Row Effect -> Bool
specContainedImpliesSubrow x y =
  allOccur (RUnion x y) ||
  not (contained x y) ||
  subrow x y

specSubrowImpliesContained :: Row Effect -> Row Effect -> Bool
specSubrowImpliesContained x y =
  allOccur (RUnion x y) ||
  not (subrow x y) ||
  contained x y

subrowSpec :: Spec
subrowSpec = describe "subrow" $ modifyMaxSuccess (const 100000) $ do
  it "returns True for x <= y if x contains all the effects in y" $
    property specContainedImpliesSubrow
  it "returns True for x <= y implies x contains all the effects in y" $
    property specSubrowImpliesContained
