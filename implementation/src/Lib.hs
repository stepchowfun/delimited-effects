module Lib (Row(..), rowEquiv) where

import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, oneof, shrink)

data Row a b = RVar b
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
  shrink (RVar x) = []
  shrink REmpty = []
  shrink (RSingleton x) = []
  shrink (RUnion x y) = [x, y] ++ [RUnion x' y' | (x', y') <- shrink (x, y)]
  shrink (RDifference x y) = [x] ++
    [RDifference x' y' | (x', y') <- shrink (x, y)]

rowEquiv :: (Eq a, Eq b) => Row a b -> Row a b -> Bool
rowEquiv x y = True
