module Lib (Row(..), subrow) where

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

-- To determine whether one row contains another, we first convert the rows
-- into propositional formulae by embedding them into a Boolean ring. Then
-- equivalence is just equality of algebraic normal forms.
data BooleanRing a b
  = BRVariable a
  | BRAtom b
  | BRFalse
  | BRTrue
  | BRAnd (BooleanRing a b) (BooleanRing a b)
  | BRXor (BooleanRing a b) (BooleanRing a b)
  deriving Eq

-- This function converts a row into a propositional formula.
embed :: Row a b -> BooleanRing a b
embed (RVariable x) = BRVariable x
embed REmpty = BRFalse
embed (RSingleton x) = BRAtom x
embed (RUnion x y) =
  BRXor (embed x) (BRXor (embed y) (BRAnd (embed x) (embed y)))
embed (RDifference x y) =
  BRXor (embed x) (BRAnd (embed x) (embed y))

-- In order to find a canonical form for a propositional formula, we need to
-- sort the elements.  Note that the constants (true/false) are defined to come
-- first; this allows us to efficiently remove them in the end after sorting.
instance (Ord a, Ord b) => Ord (BooleanRing a b) where
  compare BRFalse BRFalse = EQ
  compare BRFalse _ = LT
  compare BRTrue BRFalse = GT
  compare BRTrue BRTrue = EQ
  compare BRTrue _ = LT
  compare (BRVariable _) BRFalse = GT
  compare (BRVariable _) BRTrue = GT
  compare (BRVariable x) (BRVariable y) = compare x y
  compare (BRVariable _) _ = LT
  compare (BRAtom _) BRFalse = GT
  compare (BRAtom _) BRTrue = GT
  compare (BRAtom _) (BRVariable _) = GT
  compare (BRAtom x) (BRAtom y) = compare x y
  compare (BRAtom _) _ = LT
  compare (BRAnd _ _) BRFalse = GT
  compare (BRAnd _ _) BRTrue = GT
  compare (BRAnd _ _) (BRVariable _) = GT
  compare (BRAnd _ _) (BRAtom _) = GT
  compare (BRAnd x y) (BRAnd w v) =
    case compare x w of
      LT -> LT
      EQ -> compare y v
      GT -> GT
  compare (BRAnd _ _) _ = LT
  compare (BRXor _ _) BRFalse = GT
  compare (BRXor _ _) BRTrue = GT
  compare (BRXor _ _) (BRVariable _) = GT
  compare (BRXor _ _) (BRAtom _) = GT
  compare (BRXor _ _) (BRAnd _ _) = GT
  compare (BRXor x y) (BRXor w v) =
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
elimPairs (x : []) = x : []
elimPairs [] = []

-- This function counts the number of singletons in a list.
countSingletons :: [BooleanRing a b] -> Integer
countSingletons ((BRAtom _) : xs) = 1 + countSingletons xs
countSingletons (_ : xs) = countSingletons xs
countSingletons [] = 0

-- This function converts a propositional formula into algebraic normal form,
-- suitable for comparison.
normalize :: (Ord a, Ord b) => BooleanRing a b -> [[BooleanRing a b]]
normalize (BRVariable x) = [[BRVariable x]]
normalize (BRAtom x) = [[BRAtom x]]
normalize BRFalse = []
normalize BRTrue = [[BRTrue]]
normalize (BRAnd x y) =
  do p <- normalize x
     q <- normalize y
     let conjunction = nub (sort (p ++ q))
     if countSingletons conjunction > 1
     then return [BRFalse]
     else return $
       case conjunction of
         (BRFalse : _) -> [BRFalse]
         (BRTrue : xs) -> xs
         xs -> xs
normalize (BRXor x y) =
  delete [BRFalse] (elimPairs (sort (normalize x ++ normalize y)))

-- Finally, this is the actual subrow decision procedure.
subrow :: (Ord a, Ord b) => Row a b -> Row a b -> Bool
subrow x y = normalize (embed (RDifference x y)) == normalize (embed REmpty)
