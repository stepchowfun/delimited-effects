module Lib (Row(..), subrow) where

import Data.Set (Set)
import qualified Data.Set as Set

data Row a b
  = RVariable a
  | REmpty
  | RSingleton b
  | RUnion (Row a b) (Row a b)
  | RDifference (Row a b) (Row a b)
  deriving (Eq, Show)

data BooleanRing a b
  = BRAtom a
  | BRExclusiveAtom b -- At most one exclusive atom may be true
  | BRFalse
  | BRTrue
  | BRAnd (BooleanRing a b) (BooleanRing a b)
  | BRXor (BooleanRing a b) (BooleanRing a b)
  deriving (Eq, Ord)

isExclusiveAtom :: BooleanRing a b -> Bool
isExclusiveAtom (BRExclusiveAtom _) = True
isExclusiveAtom _ = False

embed :: Row a b -> BooleanRing a b
embed (RVariable x) = BRAtom x
embed REmpty = BRFalse
embed (RSingleton x) = BRExclusiveAtom x
embed (RUnion x y) =
  BRXor (embed x) (BRXor (embed y) (BRAnd (embed x) (embed y)))
embed (RDifference x y) =
  BRXor (embed x) (BRAnd (embed x) (embed y))

toggleMembership :: Ord a => a -> Set a -> Set a
toggleMembership z s =
  if Set.member z s
  then Set.delete z s
  else Set.insert z s

normalize :: (Ord a, Ord b) => BooleanRing a b -> Set (Set (BooleanRing a b))
normalize (BRAtom x) =
  Set.singleton (Set.singleton (BRAtom x))
normalize (BRExclusiveAtom x) =
  Set.singleton (Set.singleton (BRExclusiveAtom x))
normalize BRFalse = Set.empty
normalize BRTrue = Set.singleton Set.empty
normalize (BRAnd x y) = foldr toggleMembership Set.empty $
  do p <- Set.toList (normalize x)
     q <- Set.toList (normalize y)
     let conjunction = Set.union p q
     if Set.size (Set.filter isExclusiveAtom conjunction) > 1
     then []
     else return conjunction
normalize (BRXor x y) = Set.foldr toggleMembership (normalize x) (normalize y)

subrow :: (Ord a, Ord b) => Row a b -> Row a b -> Bool
subrow x y = normalize (embed (RDifference x y)) == normalize (embed REmpty)
