module Lib (
  Row,
  rowEquiv
) where

data Row a = RVar String
           | REmpty
           | RSingleton a
           | RUnion (Row a) (Row a)
           | RDifference (Row a) (Row a)

rowEquiv :: Eq a => Row a -> Row a -> Bool
rowEquiv x y = undefined
