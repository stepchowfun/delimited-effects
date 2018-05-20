{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- The algorithm presented here is based on the paper by Daan Leijen called
-- "HMF: Simple type inference for first-class polymorphism". That paper was
-- published in The 13th ACM SIGPLAN International Conference on Functional
-- Programming (ICFP 2008).
module Inference
  ( typeCheck
  ) where

import Control.Monad (foldM, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, evalState, get, put)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Substitution
  ( Substitution
  , applySubst
  , composeSubst
  , emptySubst
  , singletonSubst
  , substRemoveKeys
  )
import Syntax
  ( BForAll(..)
  , EVarName(..)
  , FTerm(..)
  , ITerm(..)
  , KVarName(..)
  , Kind(..)
  , TConName(..)
  , TVarName(..)
  , Type(..)
  , arrowName
  , arrowType
  , boolName
  , boolType
  , collectBinders
  , freeKVars
  , freeTCons
  , freeTVars
  , intName
  , intType
  , listName
  , listType
  , subst
  )

-- The TypeCheck monad provides:
-- 1. The ability to generate fresh variables (via State)
-- 1. The ability to read and update the context (also via State)
-- 2. The ability to throw errors (via ExceptT)
type TypeCheck
   = ExceptT String (State ( Integer
                           , Map EVarName Type
                           , Map TVarName Kind
                           , Map TConName Kind))

-- Add a term variable to the context.
withUserEVar :: EVarName -> Type -> TypeCheck a -> TypeCheck a
withUserEVar x t k = do
  (i1, cx1, ca1, cc1) <- get
  when (Map.member x cx1) $ throwError $ "Variable already exists: " ++ show x
  put (i1, Map.insert x t cx1, ca1, cc1)
  result <- k
  (i2, cx2, ca2, cc2) <- get
  put (i2, Map.delete x cx2, ca2, cc2)
  return result

-- Generate a fresh type variable and add it to the context.
freshTVar :: Kind -> TypeCheck TVarName
freshTVar k = do
  (i, cx, ca, cc) <- get
  let a = AutoTVarName i
  put (i + 1, cx, Map.insert a k ca, cc)
  return a

-- Generate a fresh type constructor and add it to the context.
freshTCon :: Kind -> TypeCheck TConName
freshTCon k = do
  (i, cx, ca, cc) <- get
  let c = AutoTConName i
  put (i + 1, cx, ca, Map.insert c k cc)
  return c

-- Generate a fresh type variable and add it to the context.
freshKVar :: TypeCheck KVarName
freshKVar = do
  (i, cx, ca, cc) <- get
  let b = AutoKVarName i
  put (i + 1, cx, ca, cc)
  return b

-- Substitute away a variable in the context.
class ContextSubst a b | a -> b where
  contextSubst :: a -> b -> TypeCheck ()

instance ContextSubst TVarName Type where
  contextSubst a t = do
    (i, cx, ca, cc) <- get
    put
      ( i
      , Map.map (subst a t) cx
      , Map.map (subst a t) ca
      , Map.map (subst a t) cc)

instance ContextSubst KVarName Kind where
  contextSubst b k = do
    (i, cx, ca, cc) <- get
    put
      ( i
      , Map.map (subst b k) cx
      , Map.map (subst b k) ca
      , Map.map (subst b k) cc)

-- Compute the most general unifier. The returned substitution is also applied
-- to the context.
class Unify a where
  unify :: a -> a -> TypeCheck Substitution

instance Unify Kind where
  unify (KVar b1) (KVar b2)
    | b1 == b2 = return emptySubst
  unify (KVar b) k
    | b `notElem` freeKVars k = do
      contextSubst b k
      return $ singletonSubst b k
  unify k (KVar b)
    | b `notElem` freeKVars k = do
      contextSubst b k
      return $ singletonSubst b k
  unify KType KType = return emptySubst
  unify k1 k2 =
    throwError $ "Unable to unify " ++ show k1 ++ " with " ++ show k2

instance Unify Type where
  unify (TVar a1) (TVar a2)
    | a1 == a2 = return emptySubst
  unify (TVar a) t
    | a `notElem` freeTVars t = do
      contextSubst a t
      return $ singletonSubst a t
  unify t (TVar a)
    | a `notElem` freeTVars t = do
      contextSubst a t
      return $ singletonSubst a t
  unify t1@(TCon c1 ts1) t2@(TCon c2 ts2)
    | c1 == c2 = do
      if length ts1 == length ts2
        then return ()
        else throwError $ "Unable to unify " ++ show t1 ++ " with " ++ show t2
      foldM
        (\theta1 (t3, t4) -> do
           theta2 <- unify (applySubst theta1 t3) (applySubst theta1 t4)
           return $ composeSubst theta1 theta2)
        emptySubst
        (zip ts1 ts2)
  unify t3@(TForAll a1 k1 t1) t4@(TForAll a2 k2 t2) = do
    theta1 <- unify k1 k2
    c <- freshTCon (applySubst theta1 k1)
    theta2 <-
      unify
        (subst a1 (TCon c []) (applySubst theta1 t1))
        (subst a2 (TCon c []) (applySubst theta1 t2))
    if c `elem` freeTCons theta2
      then throwError $ "Unable to unify " ++ show t3 ++ " with " ++ show t4
      else return (composeSubst theta1 theta2)
  unify t1 t2 =
    throwError $ "Unable to unify " ++ show t1 ++ " with " ++ show t2

-- Instantiate, generalize, and unify as necessary to make a given term and
-- type match another given type. The returned substitution is also applied to
-- the context.
subsume :: FTerm -> Type -> Type -> TypeCheck (FTerm, Substitution)
subsume e1 t1 t2 = do
  let (BForAll aks1, t3) = collectBinders t1
      (BForAll aks2, t4) = collectBinders t2
  aks3 <-
    mapM
      (\(_, k) -> do
         a <- freshTVar k
         return (a, k))
      aks1
  cks1 <-
    mapM
      (\(_, k) -> do
         c <- freshTCon k
         return (c, k))
      aks2
  let e3 = foldr (\(a, _) e2 -> FETApp e2 (TVar a)) e1 aks3
      t5 =
        foldr (\((a1, _), (a2, _)) -> subst a1 (TVar a2)) t3 (zip aks1 aks3)
      t6 = foldr (\((a, _), (c, _)) -> subst a (TCon c [])) t4 (zip aks2 cks1)
  theta1 <- unify t5 t6
  let theta2 = substRemoveKeys (Set.fromList $ fst <$> aks3) theta1
  if Set.null $
     Set.intersection
       (Set.fromList (fst <$> cks1))
       (Set.fromList $ freeTCons theta2)
    then return ()
    else throwError $ show t2 ++ " is not subsumed by " ++ show t1
  caks <-
    mapM
      (\(c, k) -> do
         a <- freshTVar k
         return (c, a, k))
      cks1
  let e5 =
        foldr
          (\(c, a, k) e4 -> FETAbs a k (subst c (TVar a) e4))
          (applySubst theta1 e3)
          caks
  return (e5, theta2)

-- Generalize a term and a type.
generalize :: FTerm -> Type -> TypeCheck (FTerm, Type)
generalize e1 t1 = do
  (_, cx, ca, _) <- get
  let cfv = Set.fromList $ Map.foldr (\t2 as -> freeTVars t2 ++ as) [] cx
      tfv = filter (`Set.notMember` cfv) $ nub $ freeTVars e1 ++ freeTVars t1
      aks =
        (\a ->
           case Map.lookup a ca of
             Just k -> (a, k)
             Nothing ->
               error $
               "Type variable " ++ show a ++ " not in context " ++ show ca) <$>
        tfv
  return
    ( foldr (\(a, k) e2 -> FETAbs a k e2) e1 aks
    , foldr (\(a, k) t2 -> TForAll a k t2) t1 aks)

-- Instantiate outer universal quantifiers with fresh type variables.
open :: FTerm -> Type -> TypeCheck (FTerm, Type)
open e (TForAll a1 k t) = do
  a2 <- freshTVar k
  open (FETApp e (TVar a2)) (subst a1 (TVar a2) t)
open e t = return (e, t)

-- Replace all variables in a type (both free and bound) with fresh variables.
-- This is used to sanitize type annotations, which would otherwise be subject
-- to issues related to variable capture (e.g., in type applications). Note
-- that "free" variables in type annotations are implicitly existentially bound
-- so they are not really free (and thus we are justified in renaming them).
sanitizeAnnotation :: Type -> TypeCheck Type
sanitizeAnnotation t1 = do
  let as = freeTVars t1
      bs = freeKVars t1
  t2 <-
    foldM
      (\t2 a1 -> do
         b <- freshKVar
         a2 <- freshTVar (KVar b)
         return (subst a1 (TVar a2) t2))
      t1
      as
  t3 <-
    foldM
      (\t3 b1 -> do
         b2 <- freshKVar
         return (subst b1 (KVar b2) t3))
      t2
      bs
  t4 <- replaceBoundVars t3
  (t5, _, _) <- infer t4
  return t5
  where
    replaceBoundVars t2@(TVar _) = return t2
    replaceBoundVars (TCon c ts1) = do
      ts2 <- mapM replaceBoundVars ts1
      return $ TCon c ts2
    replaceBoundVars (TForAll a1 k t2) = do
      a2 <- freshTVar k
      t3 <- replaceBoundVars (subst a1 (TVar a2) t2)
      return $ TForAll a2 k t3

-- Check the arities of type constructors.
class CheckArities a where
  checkArities :: a -> TypeCheck ()

instance CheckArities Type where
  checkArities (TVar _) = return ()
  checkArities (TCon c ts)
    | c == arrowName =
      if length ts == 2
        then mapM_ checkArities ts
        else throwError $
             "The (->) type constructor takes two type arguments, " ++
             "but was given " ++ show (length ts)
  checkArities (TCon c ts)
    | c == boolName =
      if null ts
        then return ()
        else throwError $
             "The " ++
             show c ++
             " type constructor takes no type arguments, " ++
             "but was given " ++ show (length ts)
  checkArities (TCon c ts)
    | c == intName =
      if null ts
        then return ()
        else throwError $
             "The " ++
             show c ++
             " type constructor takes no type arguments, " ++
             "but was given " ++ show (length ts)
  checkArities (TCon c ts)
    | c == listName =
      if length ts == 1
        then mapM_ checkArities ts
        else throwError $
             "The " ++
             show c ++
             " type constructor takes one type argument, " ++
             "but was given " ++ show (length ts)
  checkArities (TCon c _) =
    throwError $ "Unknown type constructor: " ++ show c
  checkArities (TForAll _ k t) = do
    checkArities k
    checkArities t
    return ()

instance CheckArities Kind where
  checkArities (KVar _) = return ()
  checkArities KType = return ()

-- A type class for checking. Inference may involve unification. The infer
-- function returns a substitution which is also applied to the context.
class Check a b c d | a -> b c d where
  check :: a -> b -> TypeCheck (c, d, Substitution)

instance Check ITerm Type FTerm Type where
  check e1 t1 = do
    (e2, t2, theta1) <- infer e1
    (e3, theta2) <- subsume e2 t2 (applySubst theta1 t1)
    let theta3 = composeSubst theta1 theta2
    return (e3, applySubst theta3 t1, theta3)

instance Check (ITerm, ITerm) (Type, Type) (FTerm, FTerm) (Type, Type) where
  check (e1, e2) (t1, t2) = do
    (e3, t3, theta1) <- check e1 t1
    (e4, t4, theta2) <- check e2 (applySubst theta1 t2)
    return
      ( (applySubst theta2 e3, e4)
      , (applySubst theta2 t3, t4)
      , composeSubst theta1 theta2)

instance Check Type Kind Type Kind where
  check t1 k1 = do
    (t2, k2, theta1) <- infer t1
    theta2 <- unify k1 k2
    let theta3 = composeSubst theta1 theta2
    return (applySubst theta2 t2, applySubst theta3 k1, theta3)

-- A type class for inference. Inference may involve unification. The infer
-- function returns a substitution which is also applied to the context.
class Infer a b c | a -> b c where
  infer :: a -> TypeCheck (b, c, Substitution)

instance Infer Type Type Kind where
  infer (TVar a) = do
    (_, _, ca, _) <- get
    case Map.lookup a ca of
      Just k -> return (TVar a, k, emptySubst)
      Nothing -> throwError $ "Undefined type variable: " ++ show a
  infer (TCon c ts1) = do
    (ts2, theta) <-
      foldM
        (\(ts2, theta1) t1 -> do
           (t2, _, theta2) <- check (applySubst theta1 t1) KType
           return (t2 : ts2, composeSubst theta1 theta2))
        ([], emptySubst)
        ts1
    return (TCon c (reverse ts2), KType, theta)
  infer (TForAll a1 k1 t1) = do
    a2 <- freshTVar k1
    (t2, _, theta) <- check (subst a1 (TVar a2) t1) KType
    return (TForAll a2 (applySubst theta k1) t2, KType, theta)

instance Infer ITerm FTerm Type where
  infer (IEVar x) = do
    (_, cx, _, _) <- get
    case Map.lookup x cx of
      Just t -> return (FEVar x, t, emptySubst)
      Nothing -> throwError $ "Undefined variable: " ++ show x
  infer (IEAbs x t1 e1) = do
    t2 <- sanitizeAnnotation t1
    checkArities t2
    (e2, t3, theta) <-
      withUserEVar x t2 $ do
        (e2, t3, theta1) <- infer e1
        let t4 = applySubst theta1 t2
        (t5, _, theta2) <- check t4 KType
        let theta3 = composeSubst theta1 theta2
            t6 = applySubst theta3 t3
            e3 = applySubst theta3 e2
        (e4, t7) <- open e3 t6
        case (t2, t5) of
          (TForAll {}, _) -> return ()
          (_, TForAll {}) ->
            throwError $ "Inferred polymorphic argument type: " ++ show t5
          _ -> return ()
        return (FEAbs x t5 e4, arrowType t5 t7, theta3)
    (e3, t4) <- generalize e2 t3
    return (e3, t4, theta)
  infer (IEApp e1 e2) = do
    a1 <- freshTVar KType
    a2 <- freshTVar KType
    (e3, t1, theta1) <- check e1 $ arrowType (TVar a1) (TVar a2)
    let (t4, t5) =
          case t1 of
            TCon c [t2, t3]
              | c == arrowName -> (t2, t3)
            _ -> error "Something went wrong."
    (e4, _, theta2) <- check e2 t4
    (e5, t6) <-
      generalize (FEApp (applySubst theta2 e3) e4) (applySubst theta2 t5)
    return (e5, t6, composeSubst theta1 theta2)
  infer (IELet x e1 e2) = do
    (e3, t1, theta1) <- infer e1
    withUserEVar x t1 $ do
      (e4, t2, theta2) <- infer e2
      return
        ( FEApp (FEAbs x (applySubst theta2 t1) e4) (applySubst theta2 e3)
        , t2
        , composeSubst theta1 theta2)
  infer (IEAnno e1 t1) = do
    t2 <- sanitizeAnnotation t1
    checkArities t2
    (e2, t3, theta) <- check e1 t2
    (e3, t4) <- generalize e2 t3
    return (e3, t4, theta)
  infer IETrue = return (FETrue, boolType, emptySubst)
  infer IEFalse = return (FEFalse, boolType, emptySubst)
  infer (IEIf e1 e2 e3) = do
    (e4, _, theta1) <- check e1 boolType
    t1 <- TVar <$> freshTVar KType
    ((e5, e6), (t2, _), theta2) <- check (e2, e3) (t1, t1)
    (e7, t3) <- generalize (FEIf (applySubst theta2 e4) e5 e6) t2
    return (e7, t3, composeSubst theta1 theta2)
  infer (IEIntLit i) = return (FEIntLit i, intType, emptySubst)
  infer (IEAdd e1 e2) = do
    ((e3, e4), _, theta) <- check (e1, e2) (intType, intType)
    (e5, t) <- generalize (FEAdd e3 e4) intType
    return (e5, t, theta)
  infer (IESub e1 e2) = do
    ((e3, e4), _, theta) <- check (e1, e2) (intType, intType)
    (e5, t) <- generalize (FESub e3 e4) intType
    return (e5, t, theta)
  infer (IEMul e1 e2) = do
    ((e3, e4), _, theta) <- check (e1, e2) (intType, intType)
    (e5, t) <- generalize (FEMul e3 e4) intType
    return (e5, t, theta)
  infer (IEDiv e1 e2) = do
    ((e3, e4), _, theta) <- check (e1, e2) (intType, intType)
    (e5, t) <- generalize (FEDiv e3 e4) intType
    return (e5, t, theta)
  infer (IEList es1) = do
    t1 <- TVar <$> freshTVar KType
    (es2, theta) <-
      foldM
        (\(es2, theta1) e1 -> do
           (e2, _, theta2) <- check e1 (applySubst theta1 t1)
           return
             (e2 : (applySubst theta2 <$> es2), composeSubst theta1 theta2))
        ([], emptySubst)
        es1
    (e, t2) <-
      generalize (FEList $ reverse es2) (listType (applySubst theta t1))
    return (e, t2, theta)
  infer (IEConcat e1 e2) = do
    t1 <- listType . TVar <$> freshTVar KType
    ((e3, e4), (t2, _), theta) <- check (e1, e2) (t1, t1)
    (e5, t3) <- generalize (FEConcat e3 e4) t2
    return (e5, t3, theta)

-- Type inference can generate superfluous type abstractions and applications.
-- This function removes them. The simplification is type-preserving.
simplify :: FTerm -> FTerm
simplify e@(FEVar _) = e
simplify (FEAbs x t e) = FEAbs x t (simplify e)
simplify (FEApp e1 e2) = FEApp (simplify e1) (simplify e2)
simplify (FETAbs a1 _ (FETApp e (TVar a2)))
  | a1 == a2 && a1 `notElem` freeTVars e = e
simplify (FETAbs a k e) = FETAbs a k (simplify e)
simplify (FETApp (FETAbs a _ e) t) = simplify (subst a t e)
simplify (FETApp e t) = FETApp (simplify e) t
simplify e@(FEIntLit _) = e
simplify (FEAdd e1 e2) = FEAdd (simplify e1) (simplify e2)
simplify (FESub e1 e2) = FESub (simplify e1) (simplify e2)
simplify (FEMul e1 e2) = FEMul (simplify e1) (simplify e2)
simplify (FEDiv e1 e2) = FEDiv (simplify e1) (simplify e2)
simplify FETrue = FETrue
simplify FEFalse = FEFalse
simplify (FEIf e1 e2 e3) = FEIf (simplify e1) (simplify e2) (simplify e3)
simplify (FEList es) = FEList $ simplify <$> es
simplify (FEConcat e1 e2) = FEConcat (simplify e1) (simplify e2)

-- Given a term in the untyped language, return a term in the typed language
-- together with its type.
typeCheck :: ITerm -> Either String (FTerm, Type)
typeCheck e1 =
  let result =
        evalState
          (runExceptT (infer e1))
          ( 0
          , Map.empty
          , Map.empty
          , Map.fromList
              [ (boolName, KType)
              , (intName, KType)
              , (listName, KType)
              , (arrowName, KType)
              ])
  in case result of
       Left s -> Left s
       Right (e2, t, _) -> Right (simplify e2, t)
