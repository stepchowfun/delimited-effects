module Inference (infer) where

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

-- Type and effect row inference

infer :: (Ord a, Ord b)
      => Context a b
      -> EffectMap a b
      -> Term a b
      -> Maybe (Type b)
      -> Maybe (Row b)
      -> Maybe (Type b, Row b)
-- EUnit
infer _ _ EUnit Nothing Nothing = return (TUnit, REmpty)
-- EVar
infer c _ (EVar x) Nothing Nothing =
  do (t, r) <- contextLookup c x
     return (t, r)
-- EAbs
infer c em (EAbs x e) (Just (TArrow t1 t2 r)) Nothing =
  do _ <- infer (CExtend c x t1 REmpty) em e (Just t2) (Just r)
     return ((TArrow t1 t2 r), REmpty)
infer _ _ (EAbs _ _) _ Nothing = Nothing
-- EApp
infer c em (EApp e1 e2) Nothing Nothing =
  do (t1, r1) <- infer c em e1 Nothing Nothing
     case t1 of
       TArrow t2 t3 r2 ->
         do (_, r3) <- infer c em e2 (Just t2) Nothing
            return (t3, (RUnion r1 (RUnion r2 r3)))
       _ -> Nothing
-- EProvide
infer c em (EProvide z1 zs e1 e2) Nothing Nothing =
  do (t1, r1) <- infer c em e1 Nothing Nothing
     let substitution = Map.fromList [ (z2, z1) | z2 <- zs ]
         substitutedType = substituteEffectsInType substitution t1
         substitutedRow = substituteEffectsInRow substitution r1
     (_, t2, r2) <- effectMapLookup em z1
     assert (subtype substitutedType t2)
     assert (subrow substitutedRow r2)
     (t3, r3) <- infer c em e2 Nothing Nothing
     return (t3, (addEffectsToRow (RDifference r3 (RSingleton z1)) zs))
-- EAnno
infer c em (EAnno e t r) Nothing Nothing = infer c em e (Just t) (Just r)
-- Subsumption
infer c em e (Just t1) Nothing =
  do (t2, r) <- infer c em e Nothing Nothing
     assert (subtype t2 t1)
     return (t1, r)
infer c em e Nothing (Just r1) =
  do (t, r2) <- infer c em e Nothing Nothing
     assert (subrow r2 r1)
     return (t, r1)
infer c em e (Just t1) (Just r1) =
  if all (== Nothing)
    [ do (_, r2) <- infer c em e (Just t1) Nothing
         assert (subrow r2 r1)
    , do (t2, _) <- infer c em e Nothing (Just r1)
         assert (subtype t2 t1)
    , do (t2, r2) <- infer c em e Nothing Nothing
         assert (subtype t2 t1)
         assert (subrow r2 r1) ]
  then Nothing
  else Just (t1, r1)
