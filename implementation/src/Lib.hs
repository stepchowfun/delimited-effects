module Lib (Row(..), rowEquiv) where

import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose)

data Row a b = RVar b
             | REmpty
             | RSingleton a
             | RUnion (Row a b) (Row a b)
             | RDifference (Row a b) (Row a b)
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Row a b) where
  arbitrary = do
    n <- choose (1, 5) :: Gen Int
    case n of
      1 -> do
        v <- arbitrary
        return $ RVar v
      2 -> return REmpty
      3 -> do
        e <- arbitrary
        return $ RSingleton e
      4 -> do
        x <- arbitrary
        y <- arbitrary
        return $ RUnion x y
      5 -> do
        x <- arbitrary
        y <- arbitrary
        return $ RDifference x y

rowEquiv :: (Eq a, Eq b) => Row a b -> Row a b -> Bool
rowEquiv x y = True
