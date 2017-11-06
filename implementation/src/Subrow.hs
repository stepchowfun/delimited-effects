module Subrow (subrow) where

import Data.Set (Set)
import Syntax (Row(..))
import qualified Data.Set as Set

data BooleanRing a
  = BRExclusiveAtom a -- At most one exclusive atom may be true
  | BRFalse
  | BRTrue
  | BRAnd (BooleanRing a) (BooleanRing a)
  | BRXor (BooleanRing a) (BooleanRing a)
  deriving (Eq, Ord)

isExclusiveAtom :: BooleanRing a -> Bool
isExclusiveAtom (BRExclusiveAtom _) = True
isExclusiveAtom _ = False

embed :: Row a -> BooleanRing a
embed REmpty = BRFalse
embed (RSingleton z) = BRExclusiveAtom z
embed (RUnion r1 r2) =
  BRXor (embed r1) (BRXor (embed r2) (BRAnd (embed r1) (embed r2)))
embed (RDifference r1 r2) =
  BRXor (embed r1) (BRAnd (embed r1) (embed r2))

toggleMembership :: Ord a => a -> Set a -> Set a
toggleMembership x s =
  if Set.member x s
  then Set.delete x s
  else Set.insert x s

normalize :: Ord a => BooleanRing a -> Set (Set (BooleanRing a))
normalize (BRExclusiveAtom r1) =
  Set.singleton (Set.singleton (BRExclusiveAtom r1))
normalize BRFalse = Set.empty
normalize BRTrue = Set.singleton Set.empty
normalize (BRAnd r1 r2) = foldr toggleMembership Set.empty $
  do r1' <- Set.toList (normalize r1)
     r2' <- Set.toList (normalize r2)
     let conjunction = Set.union r1' r2'
     if Set.size (Set.filter isExclusiveAtom conjunction) > 1
     then []
     else return conjunction
normalize (BRXor r1 r2) =
  Set.foldr toggleMembership (normalize r1) (normalize r2)

subrow :: Ord a => Row a -> Row a -> Bool
subrow r1 r2 =
  normalize (embed (RDifference r1 r2)) == normalize (embed REmpty)
