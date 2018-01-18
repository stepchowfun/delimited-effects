module Inference (check, infer) where

import Error (Partial, abort, assert, maybeToPartial)
import Subrow (subrow)
import Syntax
  ( Context(..)
  , EffectMap(..)
  , HoistedSet(..)
  , Row(..)
  , Term(..)
  , Type(..)
  , contextLookupKind
  , contextLookupType
  , eff
  , effectMapLookup
  , ftv
  , fv
  , substituteEffectInRow
  , substituteEffectInType
  , substituteTypeInType )
import Data.Maybe as Maybe

-- Check the type and row

check :: Context
      -> EffectMap
      -> Term
      -> Type
      -> Row
      -> Partial HoistedSet
check c em (EAbs x e) (TArrow t2 r2 t1 r1) r3 =
  do h1 <- check (CTExtend c x t2 r2) em e t1 r1
     assert
       (elem x (fv h1))
       ("Effects '" ++ show (eff h1) ++ "' cannot be hoisted outside" ++
         " the scope of variable '" ++ show x ++ ".")
     let h2 =
           if subrow (eff h1) r3
           then HEmpty
           else h1
     return h2
check _ _ (EAbs _ _) _ _ = abort
  "Checking for arrow types only succeeds on term abstractions."
check c em (ETAbs a1 e) (TForall a2 t r1) r2 =
    do assert
         (a1 == a2)
         ("Checking for universal types only succeeds when the type" ++
           " variables match in the term and type.")
       assert
         (Maybe.isJust $ contextLookupKind c a1)
         ("Type variables in a scope must be unique.")
       h1 <- check (CKExtend c a1) em e t r1
       assert
         (elem a1 (ftv h1))
         ("Effects '" ++ show (eff h1) ++ "' cannot be hoisted outside" ++
           " the scope of variable '" ++ show a1 ++ ".")
       let h2 =
             if subrow (eff h1) r2
             then HEmpty
             else h1
       return h2
check _ _ (ETAbs _ _) _ _ = abort
  "Checking for universal types only succeeds on type abstractions."
check c em (EHandle z e1 e2) t1 r1 =
  do (t2, r2) <- maybeToPartial (
         do x <- effectMapLookup em z
            contextLookupType c x
       ) ("Failed to look up the type of operation '" ++ show z ++ "'.")
     let t3 = substituteEffectInType z r1 t2
         r3 = substituteEffectInRow z r1 r2
     h1 <- check c em e1 t3 r3
     h2 <- check c em e2 t1 (RUnion r1 (RSingleton z))
     let h3 = if subrow (eff h1) r1 then HEmpty else h1
     return (HUnion h2 h3)
check c em e t1 r =
  do (t2, h) <- infer c em e r
     assert (t1 == t2) ("Subsumption cannot be applied because the types '" ++
       show t1 ++ "' and '" ++ show t2 ++ "' are not equal")
     return h

-- Infer the type and check the row

infer :: Context
      -> EffectMap
      -> Term
      -> Row
      -> Partial (Type, HoistedSet)
infer c _ (EVar x) r1 =
  do (t, r2) <- maybeToPartial (contextLookupType c x)
       ("Variable '" ++ show x ++ "' is not in the type context.")
     let h =
           if subrow r2 r1
           then HEmpty
           else HSingleton (EVar x) t r2
     return (t, h)
infer _ _ (EAbs _ _) _ = abort
  "The type of a term abstraction cannot be synthesized."
infer c em (EApp e1 e2) r3 =
  do (t3, h1) <- infer c em e1 r3
     case t3 of
       TArrow t2 r2 t1 r1 ->
         do h2 <- check c em e2 t2 r2
            let h3 =
                  case (subrow r1 r3, subrow (eff h2) r3) of
                    (True, True) -> h1
                    (True, False) -> HUnion h1 h2
                    (False, _) ->
                      HSingleton
                        (EApp e1 e2)
                        t1
                        (RUnion (RUnion (RUnion r1 r3) (eff h1)) (eff h2))
            return (t1, h3)
       _ -> abort "The type of a term applicand must be an arrow."
infer _ _ (ETAbs _ _) _ = abort
  "The type of a type abstraction cannot be synthesized."
infer c em (ETApp e t2) r2 =
  do (t3, h1) <- infer c em e r2
     case t3 of
       TForall a t1 r1 ->
         let t4 = substituteTypeInType a t2 t1
             h2 =
               if (subrow r1 r2)
               then h1
               else HSingleton (ETApp e t2) t4 (RUnion (RUnion r1 r2) (eff h1))
         in return (t4, h2)
       _ -> abort "The type of a type applicand must be a forall."
infer c em (EHandle z e1 e2) r1 =
  do (t2, r2) <- maybeToPartial (
         do x <- effectMapLookup em z
            contextLookupType c x
       ) ("Failed to look up the type of operation '" ++ show z ++ "'.")
     let t3 = substituteEffectInType z r1 t2
         r3 = substituteEffectInRow z r1 r2
     h1 <- check c em e1 t3 r3
     (t1, h2) <- infer c em e2 (RUnion r1 (RSingleton z))
     let h3 = if subrow (eff h1) r1 then HEmpty else h1
     return (t1, HUnion h2 h3)
infer c em (EAnno e t) r =
  do h <- check c em e t r
     return (t, h)
