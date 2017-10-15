module Lib (Row(..), equivalent) where

import Test.QuickCheck (Arbitrary, arbitrary, oneof, shrink)

data Row a b
  = RVar b
  | REmpty
  | RSingleton a
  | RUnion (Row a b) (Row a b)
  | RDifference (Row a b) (Row a b)
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Row a b) where
  arbitrary = oneof
    [ RVar <$> arbitrary
    , pure REmpty
    , RSingleton <$> arbitrary
    , RUnion <$> arbitrary <*> arbitrary
    , RDifference <$> arbitrary <*> arbitrary
    ]
  shrink (RVar _) = []
  shrink REmpty = []
  shrink (RSingleton _) = []
  shrink (RUnion x y) = [x, y] ++ [RUnion x' y' | (x', y') <- shrink (x, y)]
  shrink (RDifference x y) = [x] ++
    [RDifference x' y' | (x', y') <- shrink (x, y)]

normalize :: (Ord a, Ord b) => Row a b -> Row a b
normalize = error "Not yet implemented"

equivalent :: (Ord a, Ord b) => Row a b -> Row a b -> Bool
equivalent x y = normalize x == normalize y
