module Inference
  ( checkTypeAndRow
  , checkTypeInferRow
  , inferTypeAndRow
  , inferTypeCheckRow ) where

import Subrow (subrow)
import Subtype (subtype)
import Syntax
  ( Context(..)
  , EffectMap(..)
  , Row(..)
  , Term(..)
  , Type(..)
  , contextLookup
  , effectMapLookup
  , substituteEffectsInRow
  , substituteEffectsInType )
import qualified Data.Map.Lazy as Map

-- Helper functions

assert :: Bool -> Maybe ()
assert x = if x then Just () else Nothing

addEffectsToRow :: Row a -> [a] -> Row a
addEffectsToRow r [] = r
addEffectsToRow r (x : []) = RUnion (RSingleton x) r
addEffectsToRow r (x : xs) = RUnion (RSingleton x) (addEffectsToRow r xs)

-- Infer the type and row

inferTypeAndRow :: (Ord a, Ord b)
                => Context a b
                -> EffectMap a b
                -> Term a b
                -> Maybe (Type b, Row b)
inferTypeAndRow _ _ EUnit = return (TUnit, REmpty)
inferTypeAndRow c _ (EVar x) = contextLookup c x
inferTypeAndRow _ _ (EAbs _ _) = Nothing
inferTypeAndRow c em (EApp e1 e2) =
  do (t1, r1) <- inferTypeAndRow c em e1
     case t1 of
       TArrow t2 t3 r2 ->
         do r3 <- checkTypeInferRow c em e2 t2
            return (t3, (RUnion r1 (RUnion r2 r3)))
       _ -> Nothing
inferTypeAndRow c em (EProvide z1 zs e1 e2) =
  do (t1, r1) <- inferTypeAndRow c em e1
     let substitution = Map.fromList [ (z2, z1) | z2 <- zs ]
         substitutedType = substituteEffectsInType substitution t1
         substitutedRow = substituteEffectsInRow substitution r1
     (_, t2, r2) <- effectMapLookup em z1
     assert (subtype substitutedType t2)
     assert (subrow substitutedRow r2)
     (t3, r3) <- inferTypeAndRow c em e2
     return (t3, (addEffectsToRow (RDifference r3 (RSingleton z1)) zs))
inferTypeAndRow c em (EAnno e t r) =
  do checkTypeAndRow c em e t r
     return (t, r)

-- Infer the type, check the row

inferTypeCheckRow :: (Ord a, Ord b)
                  => Context a b
                  -> EffectMap a b
                  -> Term a b
                  -> Row b
                  -> Maybe (Type b)
inferTypeCheckRow c em e r1 =
  do (t, r2) <- inferTypeAndRow c em e
     assert (subrow r2 r1)
     return t

-- Check the type, infer the row

checkTypeInferRow :: (Ord a, Ord b)
                  => Context a b
                  -> EffectMap a b
                  -> Term a b
                  -> Type b
                  -> Maybe (Row b)
checkTypeInferRow c em (EAbs x e) (TArrow t1 t2 r) =
  do checkTypeAndRow (CExtend c x t1 REmpty) em e t2 r
     return REmpty
checkTypeInferRow _ _ (EAbs _ _) _ = Nothing
checkTypeInferRow c em e t1 =
  do (t2, r) <- inferTypeAndRow c em e
     assert (subtype t2 t1)
     return r

-- Check the type and row

checkTypeAndRow :: (Ord a, Ord b)
                => Context a b
                -> EffectMap a b
                -> Term a b
                -> Type b
                -> Row b
                -> Maybe ()
checkTypeAndRow c em e t1 r1 =
  if all (== Nothing)
    [ do r2 <- checkTypeInferRow c em e t1
         assert (subrow r2 r1)
    , do t2 <- inferTypeCheckRow c em e r1
         assert (subtype t2 t1)
    , do (t2, r2) <- inferTypeAndRow c em e
         assert (subtype t2 t1)
         assert (subrow r2 r1) ]
  then Nothing
  else Just ()
