module Inference
  ( checkTypeAndRow
  , checkTypeInferRow
  , inferTypeAndRow
  , inferTypeCheckRow ) where

import Error
  ( Partial
  , abort
  , assert
  , maybeToPartial
  , partialToMaybe )
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

-- Infer the type and row

inferTypeAndRow :: (Ord a, Ord b, Show a, Show b)
                => Context a b
                -> EffectMap a b
                -> Term a b
                -> Partial (Type b, Row b)
inferTypeAndRow _ _ EUnit = return (TUnit, REmpty)
inferTypeAndRow c _ (EVar x) = maybeToPartial (contextLookup c x)
  ("Variable '" ++ show x ++ "' is not in the type context.")
inferTypeAndRow _ _ (EAbs _ _) = abort
  "The type and row of an abstraction cannot be synthesized."
inferTypeAndRow c em (EApp e1 e2) =
  do (t1, r1) <- inferTypeAndRow c em e1
     case t1 of
       TArrow t2 t3 r2 ->
         do r3 <- checkTypeInferRow c em e2 t2
            return (t3, (RUnion r1 (RUnion r2 r3)))
       _ -> abort "The type of an applicand must be an arrow."
inferTypeAndRow c em (EHandle z r4 e1 e2) =
  do x <- maybeToPartial (effectMapLookup em z)
       ("Effect '" ++ show z ++ "' is not in the effect context.")
     (t3, r3) <- maybeToPartial (contextLookup c x)
       ("Operation '" ++ show x ++ "' is not in the type context.")
     let substitution = Map.singleton z r4
         t1 = substituteEffectsInType substitution t3
         r1 = substituteEffectsInRow substitution r3
     checkTypeAndRow c em e1 t1 r1
     (t2, r2) <- inferTypeAndRow c em e2
     return (t2, (RUnion (RDifference r2 (RSingleton z)) r4))
inferTypeAndRow c em (EAnno e t r) =
  do checkTypeAndRow c em e t r
     return (t, r)

-- Infer the type, check the row

inferTypeCheckRow :: (Ord a, Ord b, Show a, Show b)
                  => Context a b
                  -> EffectMap a b
                  -> Term a b
                  -> Row b
                  -> Partial (Type b)
inferTypeCheckRow c em e r1 =
  do (t, r2) <- inferTypeAndRow c em e
     assert (subrow r2 r1)
       "The synthesized row is not a subrow of the expected row."
     return t

-- Check the type, infer the row

checkTypeInferRow :: (Ord a, Ord b, Show a, Show b)
                  => Context a b
                  -> EffectMap a b
                  -> Term a b
                  -> Type b
                  -> Partial (Row b)
checkTypeInferRow c em (EAbs x e) (TArrow t1 t2 r) =
  do checkTypeAndRow (CExtend c x t1 REmpty) em e t2 r
     return REmpty
checkTypeInferRow _ _ (EAbs _ _) _ = abort
  "Checking for arrow types only succeeds on abstractions."
checkTypeInferRow c em (EHandle z r4 e1 e2) t2 =
  do x <- maybeToPartial (effectMapLookup em z)
       ("Effect '" ++ show z ++ "' is not in the effect context.")
     (t3, r3) <- maybeToPartial (contextLookup c x)
       ("Operation '" ++ show x ++ "' is not in the type context.")
     let substitution = Map.singleton z r4
         t1 = substituteEffectsInType substitution t3
         r1 = substituteEffectsInRow substitution r3
     checkTypeAndRow c em e1 t1 r1
     r2 <- checkTypeInferRow c em e2 t2
     return (RUnion (RDifference r2 (RSingleton z)) r4)
checkTypeInferRow c em e t1 =
  do (t2, r) <- inferTypeAndRow c em e
     assert (subtype t2 t1)
       "The synthesized type is not a subtype of the expected type."
     return r

-- Check the type and row

checkTypeAndRow :: (Ord a, Ord b, Show a, Show b)
                => Context a b
                -> EffectMap a b
                -> Term a b
                -> Type b
                -> Row b
                -> Partial ()
checkTypeAndRow c em e t1 r1 =
  assert (all (== Nothing)
    [ partialToMaybe $
        do r2 <- checkTypeInferRow c em e t1
           assert (subrow r2 r1)
             "The synthesized row is not a subrow of the expected row."
    , partialToMaybe $
        do t2 <- inferTypeCheckRow c em e r1
           assert (subtype t2 t1)
             "The synthesized type is not a subtype of the expected type."
    , partialToMaybe $
        do (t2, r2) <- inferTypeAndRow c em e
           assert (subtype t2 t1)
             "The synthesized type is not a subtype of the expected type."
           assert (subrow r2 r1)
             "The synthesized row is not a subrow of the expected row." ])
    "The synthesized type and row were not subsumed by the expected type and \
      \row."
