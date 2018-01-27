module Inference (check, infer) where

import Error (Partial, abort, assert, maybeToPartial)
import Subrow (subrow)
import Syntax
  ( Context(..)
  , EffectMap(..)
  , Row(..)
  , Term(..)
  , Type(..)
  , contextLookupKind
  , contextLookupType
  , effectMapLookup
  , substituteEffectInRow
  , substituteEffectInType
  , substituteTypeInType )

-- Check the type and row

check :: Context -> EffectMap -> Term -> Type -> Row -> Partial ()
check c em (EAbs x e) (TArrow t2 r2 t1 r1) _ =
  check (CTExtend c x t2 r2) em e t1 r1
check _ _ (EAbs _ _) _ _ =
  abort "Checking for arrow types only succeeds on term abstractions."
check c em (ETAbs a1 e) (TForall a2 t r1) _ = do
  assert
    (a1 == a2)
    (  "Checking for universal types only succeeds when the type"
    ++ " variables match in the term and type."
    )
  assert (contextLookupKind c a1 == Nothing)
         ("Type variables in a scope must be unique.")
  check (CKExtend c a1) em e t r1
check _ _ (ETAbs _ _) _ _ =
  abort "Checking for universal types only succeeds on type abstractions."
check c em (EEffect a x t1 r1 e) t2 r2 =
  check (CTExtend c x t1 r1) (EMExtend em a t1 r1) e t2 r2
check c em (EHandle a e1 e2) t1 r1 = do
  (t2, r2) <- maybeToPartial
    (effectMapLookup em a)
    ("Failed to look up the type of operation '" ++ show a ++ "'.")
  let t3 = substituteEffectInType a r1 t2
      r3 = substituteEffectInRow a r1 r2
  check c em e1 t3 r3
  check c em e2 t1 (RUnion r1 (RSingleton a))
check c em e t1 r = do
  t2 <- infer c em e r
  assert
    (t1 == t2)
    (  "Subsumption cannot be applied because the types '"
    ++ show t1
    ++ "' and '"
    ++ show t2
    ++ "' are not equal"
    )

-- Infer the type and check the row

infer :: Context -> EffectMap -> Term -> Row -> Partial Type
infer c _ (EVar x) r2 = do
  (t, r1) <- maybeToPartial
    (contextLookupType c x)
    ("Variable '" ++ show x ++ "' is not in the type context.")
  assert
    (subrow r1 r2)
    (  "Could not synthesize the type of a variable because the row "
    ++ show r2
    ++ " does not subsume the row "
    ++ show r1
    )
  return t
infer _ _ (EAbs _ _) _ =
  abort "The type of a term abstraction cannot be synthesized."
infer c em (EApp e1 e2) r3 = do
  t3 <- infer c em e1 r3
  case t3 of
    TArrow t2 r2 t1 r1 -> do
      check c em e2 t2 (RUnion r2 r3)
      assert
        (subrow r1 r3)
        (  "The effects from the body of an abstraction must be"
        ++ "subsumed by the effects on the application."
        )
      return t1
    _ -> abort "The type of a term applicand must be an arrow."
infer _ _ (ETAbs _ _) _ =
  abort "The type of a type abstraction cannot be synthesized."
infer c em (ETApp e t2) r2 = do
  t3 <- infer c em e r2
  case t3 of
    TForall a t1 r1 -> do
      let t4 = substituteTypeInType a t2 t1
      assert
        (subrow r1 r2)
        (  "The effects from the body of a type abstraction must be"
        ++ "subsumed by the effects on the type application."
        )
      return t4
    _ -> abort "The type of a type applicand must be a forall."
infer c em (EEffect a x t1 r1 e) r2 =
  infer (CTExtend c x t1 r1) (EMExtend em a t1 r1) e r2
infer c em (EHandle a e1 e2) r1 = do
  (t2, r2) <- maybeToPartial
    (effectMapLookup em a)
    ("Failed to look up the type of operation '" ++ show a ++ "'.")
  let t3 = substituteEffectInType a r1 t2
      r3 = substituteEffectInRow a r1 r2
  check c em e1 t3 r3
  infer c em e2 (RUnion r1 (RSingleton a))
infer c em (EAnno e t) r = do
  check c em e t r
  return t
