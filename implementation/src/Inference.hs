module Inference (check, infer) where

import Error (Partial, abort, assert, maybeToPartial)
import Subrow (subrow)
import Syntax
  ( Context(..)
  , EffectMap(..)
  , Row(..)
  , Term(..)
  , Type(..)
  , VarSet(..)
  , contextLookup
  , effectMapLookup
  , freeVars
  , substituteEffectInRow
  , substituteEffectInType
  , varSetContains )

-- Check the type and row

check :: (Ord a, Ord b, Show a, Show b)
      => Context a b
      -> EffectMap a b
      -> Term a b
      -> Type b
      -> Row b
      -> Partial (Row b, VarSet a)
check c em (EIf e1 e2 e3) t r4 =
  do (r1, v1) <- check c em e1 TBool r4
     (r2, v2) <- check c em e2 t r4
     (r3, v3) <- check c em e3 t r4
     return (RUnion r1 (RUnion r2 r3), VUnion v1 (VUnion v2 v3))
check c em (EAbs x e) (TArrow t2 r2 t1 r1) r3 =
  do (r4, v1) <- check (CExtend c x t2 r2) em e t1 r1
     assert (varSetContains x v1)
       ("Effects '" ++ show r4 ++ "' cannot be hoisted outside" ++
         " the scope of variable '" ++ show x ++ ".")
     let (r5, v2) =
           if subrow r4 r3
           then (REmpty, VEmpty)
           else (r4, v1)
     return (r5, v2)
check _ _ (EAbs _ _) _ _ = abort
  "Checking for arrow types only succeeds on abstractions."
check c em (EHandle z e1 e2) t1 r1 =
  do (t2, r2) <- maybeToPartial (
         do x <- effectMapLookup em z
            contextLookup c x
       ) ("Failed to look up the type of operation '" ++ show z ++ "'.")
     let t3 = substituteEffectInType z r1 t2
         r3 = substituteEffectInRow z r1 r2
     (r4, v1) <- check c em e1 t3 r3
     (r5, v2) <- check c em e2 t1 (RUnion r1 (RSingleton z))
     let (r6, v3) = if subrow r4 r1 then (REmpty, VEmpty) else (r4, v1)
     return (RUnion r5 r6, VUnion v2 v3)
check c em e t1 r1 =
  do (t2, r2, v) <- infer c em e r1
     assert (t1 == t2) ("Subsumption cannot be applied because the types '" ++
       show t1 ++ "' and '" ++ show t2 ++ "' are not equal")
     return (r2, v)

-- Infer the type and check the row

infer :: (Ord a, Ord b, Show a, Show b)
      => Context a b
      -> EffectMap a b
      -> Term a b
      -> Row b
      -> Partial (Type b, Row b, VarSet a)
infer _ _ ETrue _ = Right (TBool, REmpty, VEmpty)
infer _ _ EFalse _ = Right (TBool, REmpty, VEmpty)
infer c em (EIf e1 e2 e3) r4 =
  do (r1, v1) <- check c em e1 TBool r4
     (t1, r2, v2) <- infer c em e2 r4
     (t2, r3, v3) <- infer c em e3 r4
     assert (t1 == t2)
       ("Types '" ++ show t1 ++ "' and '" ++ show t2 ++ "' are not equal.")
     return (t1, RUnion r1 (RUnion r2 r3), VUnion v1 (VUnion v2 v3))
infer c _ (EVar x) r1 =
  do (t, r2) <- maybeToPartial (contextLookup c x)
       ("Variable '" ++ show x ++ "' is not in the type context.")
     let (r3, v) =
           if subrow r2 r1
           then (REmpty, VEmpty)
           else (r2, VSingleton x)
     return (t, r3, v)
infer _ _ (EAbs _ _) _ = abort
  "The type of an abstraction cannot be synthesized."
infer c em (EApp e1 e2) r3 =
  do (t3, r4, v1) <- infer c em e1 r3
     case t3 of
       TArrow t2 r2 t1 r1 ->
         do (r5, v2) <- check c em e2 t2 r2
            let (r6, v3) =
                  case (subrow r1 r3, subrow r5 r3) of
                    (True, True) -> (r4, v1)
                    (True, False) -> (RUnion r4 r5, VUnion v1 v2)
                    (False, _) ->
                      (RUnion (RUnion (RUnion r1 r3) r4) r5,
                      freeVars (EApp e1 e2))
            return (t1, r6, v3)
       _ -> abort "The type of an applicand must be an arrow."
infer c em (EHandle z e1 e2) r1 =
  do (t2, r2) <- maybeToPartial (
         do x <- effectMapLookup em z
            contextLookup c x
       ) ("Failed to look up the type of operation '" ++ show z ++ "'.")
     let t3 = substituteEffectInType z r1 t2
         r3 = substituteEffectInRow z r1 r2
     (r4, v1) <- check c em e1 t3 r3
     (t1, r5, v2) <- infer c em e2 (RUnion r1 (RSingleton z))
     let (r6, v3) = if subrow r4 r1 then (REmpty, VEmpty) else (r4, v1)
     return (t1, RUnion r5 r6, VUnion v2 v3)
infer c em (EAnno e t) r1 =
  do (r2, v) <- check c em e t r1
     return (t, r2, v)
