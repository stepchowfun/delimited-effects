{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- The algorithm presented here is based on the paper by Daan Leijen called
-- "HMF: Simple type inference for first-class polymorphism". That paper was
-- published in The 13th ACM SIGPLAN International Conference on Functional
-- Programming (ICFP 2008).
module Inference
  ( typeCheck
  ) where

import Control.Monad (foldM, replicateM, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, evalState, get, put)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
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
  , TConstName(..)
  , TVarName(..)
  , Type(..)
  , boolName
  , boolType
  , collectBinders
  , freeTConsts
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
                           , Set TVarName
                           , Map TConstName Integer))

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
freshTVar :: TypeCheck TVarName
freshTVar = do
  (i, cx, ca, cc) <- get
  let a = AutoTVarName i
  put (i + 1, cx, Set.insert a ca, cc)
  return a

-- Generate a fresh type constant and add it to the context.
freshTConst :: Integer -> TypeCheck TConstName
freshTConst n = do
  (i, cx, ca, cc) <- get
  let c = AutoTConstName i
  put (i + 1, cx, ca, Map.insert c n cc)
  return c

-- Substitute a type for a type variable in the context.
substInContext :: TVarName -> Type -> TypeCheck ()
substInContext a t = do
  (i, cx, ca, cc) <- get
  put (i, Map.map (subst a t) cx, ca, cc)

-- Compute the most general unifier of two types. The returned substitution is
-- also applied to the context.
unify :: Type -> Type -> TypeCheck Substitution
unify (TVar a1) (TVar a2)
  | a1 == a2 = return emptySubst
unify (TVar a) t
  | a `notElem` freeTVars t = do
    substInContext a t
    return $ singletonSubst a t
unify t (TVar a)
  | a `notElem` freeTVars t = do
    substInContext a t
    return $ singletonSubst a t
unify t1@(TConst c1 ts1) t2@(TConst c2 ts2)
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
unify (TArrow t1 t2) (TArrow t3 t4) = do
  theta1 <- unify t1 t3
  theta2 <- unify (applySubst theta1 t2) (applySubst theta1 t4)
  return $ composeSubst theta1 theta2
unify t3@(TForAll a1 t1) t4@(TForAll a2 t2) = do
  c <- freshTConst 0
  theta <- unify (subst a1 (TConst c []) t1) (subst a2 (TConst c []) t2)
  if c `elem` freeTConsts theta
    then throwError $ "Unable to unify " ++ show t3 ++ " with " ++ show t4
    else return theta
unify t1 t2 =
  throwError $ "Unable to unify " ++ show t1 ++ " with " ++ show t2

-- Instantiate, generalize, and unify as necessary to make a given term and
-- type match another given type. The returned substitution is also applied to
-- the context.
subsume :: FTerm -> Type -> Type -> TypeCheck (FTerm, Substitution)
subsume e1 t1 t2 = do
  let (BForAll as1, t3) = collectBinders t1
      (BForAll as2, t4) = collectBinders t2
  as3 <- replicateM (length as1) freshTVar
  cs1 <- replicateM (length as2) (freshTConst 0)
  let e3 = foldr (\a e2 -> FETApp e2 (TVar a)) e1 as3
      t5 = foldr (\(a1, a2) -> subst a1 (TVar a2)) t3 (zip as1 as3)
      t6 = foldr (\(a, c) -> subst a (TConst c [])) t4 (zip as2 cs1)
  theta1 <- unify t5 t6
  let theta2 = substRemoveKeys (Set.fromList as3) theta1
  if Set.null $
     Set.intersection (Set.fromList cs1) (Set.fromList $ freeTConsts theta2)
    then return ()
    else throwError $ show t2 ++ " is not subsumed by " ++ show t1
  as4 <- replicateM (length cs1) freshTVar
  let e5 =
        foldr
          (\(c, a) e4 -> FETAbs a (subst c (TVar a) e4))
          (applySubst theta1 e3)
          (zip cs1 as4)
  return (e5, theta2)

-- Generalize a term and a type.
generalize :: FTerm -> Type -> TypeCheck (FTerm, Type)
generalize e1 t1 = do
  (_, cx, _, _) <- get
  let cfv = Set.fromList $ Map.foldr (\t2 as -> freeTVars t2 ++ as) [] cx
      tfv = filter (`Set.notMember` cfv) $ nub $ freeTVars e1 ++ freeTVars t1
  return (foldr FETAbs e1 tfv, foldr TForAll t1 tfv)

-- Instantiate outer universal quantifiers with fresh type variables.
open :: FTerm -> Type -> TypeCheck (FTerm, Type)
open e (TForAll a1 t) = do
  a2 <- freshTVar
  open (FETApp e (TVar a2)) (subst a1 (TVar a2) t)
open e t = return (e, t)

-- A helper method for checking a term against a type.
check :: ITerm -> Type -> TypeCheck (FTerm, Substitution)
check e1 t1 = do
  (e2, t2, theta1) <- infer e1
  (e3, theta2) <- subsume e2 t2 (applySubst theta1 t1)
  return (e3, composeSubst theta1 theta2)

-- A helper method for type checking binary operations (e.g., arithmetic
-- operations).
checkBinary ::
     ITerm -> Type -> ITerm -> Type -> TypeCheck (FTerm, FTerm, Substitution)
checkBinary e1 t1 e2 t2 = do
  (e3, theta1) <- check e1 t1
  (e4, theta2) <- check (applySubst theta1 e2) (applySubst theta1 t2)
  return (applySubst theta2 e3, e4, composeSubst theta1 theta2)

-- Replace all variables in a type (both free and bound) with fresh type
-- variables. This is used to sanitize type annotations, which would otherwise
-- be subject to issues related to variable capture (e.g., in type
-- applications).
sanitizeAnnotation :: Type -> TypeCheck Type
sanitizeAnnotation t1 =
  let fv = freeTVars t1
  in do t2 <-
          foldM
            (\t2 a1 -> do
               a2 <- freshTVar
               return (subst a1 (TVar a2) t2))
            t1
            fv
        replaceBoundVars t2
  where
    replaceBoundVars t2@(TVar _) = return t2
    replaceBoundVars (TConst c ts1) = do
      ts2 <- mapM replaceBoundVars ts1
      return $ TConst c ts2
    replaceBoundVars (TArrow t2 t3) = do
      t4 <- replaceBoundVars t2
      t5 <- replaceBoundVars t3
      return $ TArrow t4 t5
    replaceBoundVars (TForAll a1 t2) = do
      a2 <- freshTVar
      return $ TForAll a2 (subst a1 (TVar a2) t2)

-- Infer the type of a term. Inference may involve unification. This function
-- returns a substitution which is also applied to the context.
infer :: ITerm -> TypeCheck (FTerm, Type, Substitution)
infer (IEVar x) = do
  (_, cx, _, _) <- get
  case Map.lookup x cx of
    Just t -> return (FEVar x, t, emptySubst)
    Nothing -> throwError $ "Undefined variable: " ++ show x
infer (IEAbs x t1 e1) = do
  t2 <-
    case t1 of
      Just t2 -> sanitizeAnnotation t2
      Nothing -> TVar <$> freshTVar
  (e2, t3, theta) <-
    withUserEVar x t2 $ do
      (e2, t3, theta) <- infer e1
      let t4 = applySubst theta t2
      case (t2, t4) of
        (TForAll _ _, _) -> return ()
        (_, TForAll _ _) ->
          throwError $ "Inferred polymorphic argument type: " ++ show t4
        _ -> return ()
      (e3, t5) <- open e2 t3
      return (FEAbs x t4 e3, TArrow t4 t5, theta)
  (e3, t4) <- generalize e2 t3
  return (e3, t4, theta)
infer (IEApp e1 e2) = do
  a1 <- freshTVar
  a2 <- freshTVar
  let (t1, t2) = (TVar a1, TVar a2)
  (e3, theta1) <- check e1 (TArrow (TVar a1) (TVar a2))
  let (t3, t4) = (applySubst theta1 t1, applySubst theta1 t2)
  (e4, theta2) <- check (applySubst theta1 e2) t3
  (e5, t5) <-
    generalize (FEApp (applySubst theta2 e3) e4) (applySubst theta2 t4)
  return (e5, t5, composeSubst theta1 theta2)
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
  (e2, theta) <- check e1 t2
  (e3, t3) <- generalize e2 t2
  return (e3, t3, theta)
infer IETrue = return (FETrue, boolType, emptySubst)
infer IEFalse = return (FEFalse, boolType, emptySubst)
infer (IEIf e1 e2 e3) = do
  (e4, theta1) <- check e1 boolType
  a <- freshTVar
  let t1 = TVar a
  (e5, e6, theta2) <-
    checkBinary (applySubst theta1 e2) t1 (applySubst theta1 e3) t1
  (e7, t2) <-
    generalize (FEIf (applySubst theta2 e4) e5 e6) (applySubst theta2 t1)
  return (e7, t2, composeSubst theta1 theta2)
infer (IEIntLit i) = return (FEIntLit i, intType, emptySubst)
infer (IEAdd e1 e2) = do
  (e3, e4, theta) <- checkBinary e1 intType e2 intType
  (e5, t) <- generalize (FEAddInt e3 e4) intType
  return (e5, t, theta)
infer (IESub e1 e2) = do
  (e3, e4, theta) <- checkBinary e1 intType e2 intType
  (e5, t) <- generalize (FESubInt e3 e4) intType
  return (e5, t, theta)
infer (IEMul e1 e2) = do
  (e3, e4, theta) <- checkBinary e1 intType e2 intType
  (e5, t) <- generalize (FEMulInt e3 e4) intType
  return (e5, t, theta)
infer (IEDiv e1 e2) = do
  (e3, e4, theta) <- checkBinary e1 intType e2 intType
  (e5, t) <- generalize (FEDivInt e3 e4) intType
  return (e5, t, theta)
infer (IEList es1) = do
  a <- freshTVar
  let t1 = TVar a
  (es2, theta) <-
    foldM
      (\(es2, theta1) e1 -> do
         (e2, theta2) <- check e1 (applySubst theta1 t1)
         return (e2 : (applySubst theta2 <$> es2), composeSubst theta1 theta2))
      ([], emptySubst)
      es1
  (e, t2) <-
    generalize (FEList $ reverse es2) (listType (applySubst theta t1))
  return (e, t2, theta)
infer (IEConcat e1 e2) = do
  a <- freshTVar
  let t1 = listType $ TVar a
  (e3, e4, theta) <- checkBinary e1 t1 e2 t1
  (e5, t2) <- generalize (FEConcat e3 e4) (applySubst theta t1)
  return (e5, t2, theta)

-- Type inference can generate superfluous type abstractions and applications.
-- This function removes them. This function is type-preserving.
simplify :: FTerm -> FTerm
simplify e@(FEVar _) = e
simplify (FEAbs x t e) = FEAbs x t (simplify e)
simplify (FEApp e1 e2) = FEApp (simplify e1) (simplify e2)
simplify (FETAbs a1 (FETApp e (TVar a2)))
  | a1 == a2 && a1 `notElem` freeTVars e = e
simplify (FETAbs a e) = FETAbs a (simplify e)
simplify (FETApp (FETAbs a e) t) = simplify (subst a t e)
simplify (FETApp e t) = FETApp (simplify e) t
simplify e@(FEIntLit _) = e
simplify (FEAddInt e1 e2) = FEAddInt (simplify e1) (simplify e2)
simplify (FESubInt e1 e2) = FESubInt (simplify e1) (simplify e2)
simplify (FEMulInt e1 e2) = FEMulInt (simplify e1) (simplify e2)
simplify (FEDivInt e1 e2) = FEDivInt (simplify e1) (simplify e2)
simplify FETrue = FETrue
simplify FEFalse = FEFalse
simplify (FEIf e1 e2 e3) = FEIf (simplify e1) (simplify e2) (simplify e3)
simplify (FEList es) = FEList $ simplify <$> es
simplify (FEConcat e1 e2) = FEConcat (simplify e1) (simplify e2)

-- Generate a fresh type variable name.
freshUserTVarName :: Set TVarName -> TVarName
freshUserTVarName as =
  let names =
        ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ"] ++
        ((++ "′") <$> names)
  in UserTVarName $
     head $ dropWhile (\a -> Set.member (UserTVarName a) as) names

-- Get all the "user" type variables of a term, not just the free type ones.
allTVarsTerm :: FTerm -> Set TVarName
allTVarsTerm (FEVar _) = Set.empty
allTVarsTerm (FEAbs _ t e) = Set.union (allTVarsType t) (allTVarsTerm e)
allTVarsTerm (FEApp e1 e2) = Set.union (allTVarsTerm e1) (allTVarsTerm e2)
allTVarsTerm (FETAbs a e) = Set.insert a (allTVarsTerm e)
allTVarsTerm (FETApp e t) = Set.union (allTVarsTerm e) (allTVarsType t)
allTVarsTerm FETrue = Set.empty
allTVarsTerm FEFalse = Set.empty
allTVarsTerm (FEIf e1 e2 e3) =
  allTVarsTerm e1 `Set.union` allTVarsTerm e2 `Set.union` allTVarsTerm e3
allTVarsTerm (FEIntLit _) = Set.empty
allTVarsTerm (FEAddInt e1 e2) = Set.union (allTVarsTerm e1) (allTVarsTerm e2)
allTVarsTerm (FESubInt e1 e2) = Set.union (allTVarsTerm e1) (allTVarsTerm e2)
allTVarsTerm (FEMulInt e1 e2) = Set.union (allTVarsTerm e1) (allTVarsTerm e2)
allTVarsTerm (FEDivInt e1 e2) = Set.union (allTVarsTerm e1) (allTVarsTerm e2)
allTVarsTerm (FEList es) =
  foldr (\e as -> Set.union (allTVarsTerm e) as) Set.empty es
allTVarsTerm (FEConcat e1 e2) = Set.union (allTVarsTerm e1) (allTVarsTerm e2)

-- Get all the "user" type variables of a type, not just the free type ones.
allTVarsType :: Type -> Set TVarName
allTVarsType (TVar a) = Set.singleton a
allTVarsType (TConst _ ts) =
  foldr (\t as -> Set.union (allTVarsType t) as) Set.empty ts
allTVarsType (TArrow t1 t2) = Set.union (allTVarsType t1) (allTVarsType t2)
allTVarsType (TForAll a t) = Set.insert a (allTVarsType t)

-- Convert "auto" type variables into "user" type variables for nicer printing.
-- This version of the function operates on terms.
elimAutoVarsTerm :: Set TVarName -> FTerm -> FTerm
elimAutoVarsTerm _ e@(FEVar _) = e
elimAutoVarsTerm as (FEAbs x t e) =
  FEAbs x (elimAutoVarsType as t) (elimAutoVarsTerm as e)
elimAutoVarsTerm as (FEApp e1 e2) =
  FEApp (elimAutoVarsTerm as e1) (elimAutoVarsTerm as e2)
elimAutoVarsTerm as (FETAbs a1@(AutoTVarName _) e) =
  let a2 = freshUserTVarName as
  in FETAbs a2 (elimAutoVarsTerm (Set.insert a2 as) (subst a1 (TVar a2) e))
elimAutoVarsTerm as (FETAbs a@(UserTVarName _) e) =
  FETAbs a (elimAutoVarsTerm (Set.insert a as) e)
elimAutoVarsTerm as (FETApp e t) =
  FETApp (elimAutoVarsTerm as e) (elimAutoVarsType as t)
elimAutoVarsTerm _ FETrue = FETrue
elimAutoVarsTerm _ FEFalse = FEFalse
elimAutoVarsTerm as (FEIf e1 e2 e3) =
  FEIf
    (elimAutoVarsTerm as e1)
    (elimAutoVarsTerm as e2)
    (elimAutoVarsTerm as e3)
elimAutoVarsTerm _ e@(FEIntLit _) = e
elimAutoVarsTerm as (FEAddInt e1 e2) =
  FEAddInt (elimAutoVarsTerm as e1) (elimAutoVarsTerm as e2)
elimAutoVarsTerm as (FESubInt e1 e2) =
  FESubInt (elimAutoVarsTerm as e1) (elimAutoVarsTerm as e2)
elimAutoVarsTerm as (FEMulInt e1 e2) =
  FEMulInt (elimAutoVarsTerm as e1) (elimAutoVarsTerm as e2)
elimAutoVarsTerm as (FEDivInt e1 e2) =
  FEDivInt (elimAutoVarsTerm as e1) (elimAutoVarsTerm as e2)
elimAutoVarsTerm as (FEList es) = FEList $ elimAutoVarsTerm as <$> es
elimAutoVarsTerm as (FEConcat e1 e2) =
  FEConcat (elimAutoVarsTerm as e1) (elimAutoVarsTerm as e2)

-- Convert "auto" type variables into "user" type variables for nicer printing.
-- This version of the function operates on types.
elimAutoVarsType :: Set TVarName -> Type -> Type
elimAutoVarsType _ t@(TVar _) = t
elimAutoVarsType as (TConst c ts) = TConst c (elimAutoVarsType as <$> ts)
elimAutoVarsType as (TArrow t1 t2) =
  TArrow (elimAutoVarsType as t1) (elimAutoVarsType as t2)
elimAutoVarsType as (TForAll a1@(AutoTVarName _) t) =
  let a2 = freshUserTVarName as
  in TForAll a2 (elimAutoVarsType (Set.insert a2 as) (subst a1 (TVar a2) t))
elimAutoVarsType as (TForAll a@(UserTVarName _) t) =
  TForAll a (elimAutoVarsType (Set.insert a as) t)

-- Given a term in the untyped language, return a term in the typed language
-- together with its type.
typeCheck :: ITerm -> Either String (FTerm, Type)
typeCheck e1 =
  let result =
        evalState
          (runExceptT (infer e1))
          ( 0
          , Map.empty
          , Set.empty
          , Map.fromList [(boolName, 0), (intName, 0), (listName, 1)])
  in case result of
       Left s -> Left s
       Right (e2, t, _) ->
         let e3 = simplify e2
             as = Set.union (allTVarsTerm e3) (allTVarsType t)
         in Right (elimAutoVarsTerm as e3, elimAutoVarsType as t)
