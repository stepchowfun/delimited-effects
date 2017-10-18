module Lib (Row(..), equivalent) where

import Data.List (delete, nub, sort)

-- A row denotes some set of effects. Rows may contain variables which stand
-- for other rows.
data Row a b
  = RVariable a
  | REmpty
  | RSingleton b
  | RUnion (Row a b) (Row a b)
  | RDifference (Row a b) (Row a b)
  deriving (Eq, Show)

-- To determine whether two rows are equivalent, we first convert them into
-- propositional formulae by embedding them into a Boolean ring. Then
-- equivalence is just equality of algebraic normal forms.
data BooleanRing a b
  = BAVariable a
  | BASingleton b
  | BAFalse
  | BATrue
  | BAAnd (BooleanRing a b) (BooleanRing a b)
  | BAXor (BooleanRing a b) (BooleanRing a b)
  deriving (Eq, Show)

-- This function converts a row into a propositional formula.
embed :: Row a b -> BooleanRing a b
embed (RVariable x) = BAVariable x
embed REmpty = BAFalse
embed (RSingleton x) = BASingleton x
embed (RUnion x y) =
  BAXor (embed x) (BAXor (embed y) (BAAnd (embed x) (embed y)))
embed (RDifference x y) =
  BAXor (embed x) (BAAnd (embed x) (embed y))

-- In order to find a canonical form for a propositional formula, we need to
-- sort the elements.  Note that the constants (true/false) are defined to come
-- first; this allows us to efficiently remove them in the end after sorting.
instance (Ord a, Ord b) => Ord (BooleanRing a b) where
  compare BAFalse BAFalse = EQ
  compare BAFalse _ = LT
  compare BATrue BAFalse = GT
  compare BATrue BATrue = EQ
  compare BATrue _ = LT
  compare (BAVariable _) BAFalse = GT
  compare (BAVariable _) BATrue = GT
  compare (BAVariable x) (BAVariable y) = compare x y
  compare (BAVariable _) _ = LT
  compare (BASingleton _) BAFalse = GT
  compare (BASingleton _) BATrue = GT
  compare (BASingleton _) (BAVariable _) = GT
  compare (BASingleton x) (BASingleton y) = compare x y
  compare (BASingleton _) _ = LT
  compare (BAAnd _ _) BAFalse = GT
  compare (BAAnd _ _) BATrue = GT
  compare (BAAnd _ _) (BAVariable _) = GT
  compare (BAAnd _ _) (BASingleton _) = GT
  compare (BAAnd x y) (BAAnd w v) =
    case compare x w of
      LT -> LT
      EQ -> compare y v
      GT -> GT
  compare (BAAnd _ _) _ = LT
  compare (BAXor _ _) BAFalse = GT
  compare (BAXor _ _) BATrue = GT
  compare (BAXor _ _) (BAVariable _) = GT
  compare (BAXor _ _) (BASingleton _) = GT
  compare (BAXor _ _) (BAAnd _ _) = GT
  compare (BAXor x y) (BAXor w v) =
    case compare x w of
      LT -> LT
      EQ -> compare y v
      GT -> GT

-- This function removes identical pairs of elements from a list. Note that
-- this is different from deduplication. The input is assumed to be sorted.
elimPairs :: Eq a => [a] -> [a]
elimPairs (x : y : xs) =
  if x == y
  then elimPairs xs
  else x : elimPairs (y : xs)
elimPairs (x : xs) = x : elimPairs xs
elimPairs [] = []

-- This function counts the number of singletons in a list.
countSingletons :: [BooleanRing a b] -> Integer
countSingletons ((BASingleton _) : xs) = 1 + countSingletons xs
countSingletons (_ : xs) = countSingletons xs
countSingletons [] = 0

-- This function converts a propositional formula into algebraic normal form,
-- suitable for comparison.
normalize :: (Ord a, Ord b) => BooleanRing a b -> [[BooleanRing a b]]
normalize (BAVariable x) = [[BAVariable x]]
normalize (BASingleton x) = [[BASingleton x]]
normalize BAFalse = []
normalize BATrue = [[BATrue]]
normalize (BAAnd x y) =
  do
    p <- normalize x
    q <- normalize y
    let conjunction = nub (sort (p ++ q))
    if countSingletons conjunction > 1
    then return [BAFalse]
    else return $
      case conjunction of
        (BAFalse : _) -> [BAFalse]
        (BATrue : xs) -> xs
        xs -> xs
normalize (BAXor x y) =
  delete [BAFalse] (elimPairs (sort ((normalize x) ++ (normalize y))))

-- Finally, this is the actual decision procedure for row equivalence.
equivalent :: (Ord a, Ord b) => Row a b -> Row a b -> Bool
equivalent x y = normalize (embed x) == normalize (embed y)
