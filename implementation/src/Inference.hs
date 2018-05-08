{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- The algorithm presented here is based on the paper by Daan Leijen called
-- "HMF: Simple type inference for first-class polymorphism". That paper was
-- published in The 13th ACM SIGPLAN International Conference on Functional
-- Programming (ICFP 2008).
module Inference
  ( typeCheck
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, evalState, get, put)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax
  ( BExists(..)
  , BForAll(..)
  , EVarName(..)
  , FTerm(..)
  , ITerm(..)
  , TConstName(..)
  , TVarName(..)
  , Type(..)
  , collectBinders
  , freeTConsts
  , freeTVars
  , subst
  )

-- Contexts can hold term variables, type variables, and type constants.
type Context = (Map EVarName Type, Set TVarName, Set TConstName)

intName :: TConstName
intName = TConstName "Int"

intType :: Type
intType = TConst intName

initialContext :: Context
initialContext = (Map.empty, Set.empty, Set.fromList [intName])

-- The TypeCheck monad provides:
-- 1. The ability to read and update the context (via State).
-- 2. The ability to throw errors (via ExceptT).
type TypeCheck = ExceptT String (State Context)

-- Generate a fresh term variable and add it to the context.
freshEVar :: Maybe String -> Type -> TypeCheck EVarName
freshEVar x1 t = do
  (cx, ca, cc) <- get
  let names =
        ["x", "y", "z", "u", "v", "m", "n", "p", "q"] ++ ((++ "′") <$> names)
      x2 =
        EVarName $
        head $
        dropWhile
          (\x3 -> Map.member (EVarName x3) cx)
          (case x1 of
             Just x3 -> x3 : names
             Nothing -> names)
  put (Map.insert x2 t cx, ca, cc)
  return x2

-- Generate a fresh type variable and add it to the context.
freshTVar :: Maybe String -> TypeCheck TVarName
freshTVar a1 = do
  (cx, ca, cc) <- get
  let names =
        ["a", "b", "c", "d", "e", "f", "g", "h", "i"] ++ ((++ "′") <$> names)
      a2 =
        TVarName $
        head $
        dropWhile
          (\a3 -> Set.member (TVarName a3) ca)
          (case a1 of
             Just a3 -> a3 : names
             Nothing -> names)
  put (cx, Set.insert a2 ca, cc)
  return a2

-- Generate a fresh type constant and add it to the context.
freshTConst :: Maybe String -> TypeCheck TConstName
freshTConst c1 = do
  (cx, ca, cc) <- get
  let names =
        ["A", "B", "C", "D", "E", "F", "G", "H", "I"] ++ ((++ "′") <$> names)
      c2 =
        TConstName $
        head $
        dropWhile
          (\c3 -> Set.member (TConstName c3) cc)
          (case c1 of
             Just c3 -> c3 : names
             Nothing -> names)
  put (cx, ca, Set.insert c2 cc)
  return c2

-- Delete a term variable from the context.
deleteEVar :: EVarName -> TypeCheck ()
deleteEVar x = do
  (cx, ca, cc) <- get
  put (Map.delete x cx, ca, cc)

-- Delete a type variable from the context.
deleteTVar :: TVarName -> TypeCheck ()
deleteTVar a = do
  (cx, ca, cc) <- get
  if a `elem` Map.foldr (\t as -> freeTVars t ++ as) [] cx
    then throwError $
         "Cannot delete " ++
         show a ++ " from the context because it is used by another binding"
    else put (cx, Set.delete a ca, cc)

-- Delete a type constant from the context.
deleteTConst :: TConstName -> TypeCheck ()
deleteTConst c = do
  (cx, ca, cc) <- get
  if c `elem` Map.foldr (\t cs -> freeTConsts t ++ cs) [] cx
    then throwError $
         "Cannot delete " ++
         show c ++ " from the context because it is used by another binding"
    else put (cx, ca, Set.delete c cc)

-- A substitution maps type variables to types. We maintain the invariant that
-- all substitutions are idempotent.
type Substitution = Map TVarName Type

-- Compose two substitutions. The substitutions are in diagrammatic order, that
-- is, theta2 comes after theta1.
composeSubst :: Substitution -> Substitution -> Substitution
composeSubst theta1 theta2 =
  Map.union theta2 (Map.map (applySubst theta2) theta1)

-- Substitutions can be applied to various entities.
class ApplySubst a where
  applySubst :: Substitution -> a -> a

instance ApplySubst FTerm where
  applySubst theta e = Map.foldrWithKey subst e theta

instance ApplySubst Type where
  applySubst theta t = Map.foldrWithKey subst t theta

-- Substitute a type for a type variable in the context.
substInContext :: TVarName -> Type -> TypeCheck ()
substInContext a t = do
  (cx, ca, cc) <- get
  put (Map.map (subst a t) cx, ca, cc)

-- A helper function to unify two quantified types.
unifyQuantifier ::
     TVarName -> Type -> TVarName -> Type -> String -> TypeCheck Substitution
unifyQuantifier a1 t1 a2 t2 s = do
  c <- freshTConst Nothing
  theta <- unify (subst a1 (TConst c) t1) (subst a2 (TConst c) t2)
  if c `elem` Map.foldr (\t5 fc -> fc ++ freeTConsts t5) [] theta
    then throwError s
    else do
      deleteTConst c
      return theta

-- Compute the most general unifier of two types. The returned substitution is
-- also applied to the context.
unify :: Type -> Type -> TypeCheck Substitution
unify (TVar a1) (TVar a2)
  | a1 == a2 = return Map.empty
unify (TVar a) t
  | a `notElem` freeTVars t = do
    substInContext a t
    return $ Map.singleton a t
unify t (TVar a)
  | a `notElem` freeTVars t = do
    substInContext a t
    return $ Map.singleton a t
unify (TConst c1) (TConst c2)
  | c1 == c2 = return Map.empty
unify (TArrow t1 t2) (TArrow t3 t4) = do
  theta1 <- unify t1 t3
  theta2 <- unify (applySubst theta1 t2) (applySubst theta1 t4)
  return $ composeSubst theta1 theta2
unify t3@(TForAll a1 t1) t4@(TForAll a2 t2) =
  unifyQuantifier a1 t1 a2 t2 $
  "Unable to unify " ++ show t3 ++ " with " ++ show t4
unify t3@(TExists a1 t1) t4@(TExists a2 t2) =
  unifyQuantifier a1 t1 a2 t2 $
  "Unable to unify " ++ show t3 ++ " with " ++ show t4
unify t1 t2 =
  throwError $ "Unable to unify " ++ show t1 ++ " with " ++ show t2

-- Instantiate, generalize, and unify as necessary to make a given term and
-- type match another given type. The returned substitution is also applied to
-- the context.
subsume :: FTerm -> Type -> Type -> TypeCheck (FTerm, Substitution)
subsume e1 t1 t2 = do
  let (BForAll as1, t3) = collectBinders t1
      (BForAll as2, t4) = collectBinders t2
  as3 <- mapM (\a -> freshTVar (Just $ fromTVarName a)) as1
  cs1 <- mapM (\a -> freshTConst (Just $ fromTVarName a)) as2
  let e3 = foldr (\a e2 -> FETApp e2 (TVar a)) e1 as3
      t5 = foldr (\(a1, a2) -> subst a1 (TVar a2)) t3 (zip as1 as3)
      t6 = foldr (\(a, c) -> subst a (TConst c)) t4 (zip as2 cs1)
  theta1 <- unify t5 t6
  let theta2 = Map.withoutKeys theta1 (Set.fromList as3)
  if Set.null $
     Set.intersection
       (Set.fromList cs1)
       (Map.foldr
          (\t7 cs -> Set.union (Set.fromList $ freeTConsts t7) cs)
          Set.empty
          theta2)
    then return ()
    else throwError $ show t2 ++ " is not subsumed by " ++ show t1
  as4 <- mapM (\c -> freshTVar (Just $ fromTConstName c)) cs1
  let e5 =
        foldr
          (\(c, a) e4 -> FETAbs a (subst c (TVar a) e4))
          (applySubst theta1 e3)
          (zip cs1 as4)
  mapM_ deleteTConst cs1
  mapM_ deleteTVar as4
  return (e5, theta2)

-- Generalize a term and a type.
generalize :: FTerm -> Type -> TypeCheck (FTerm, Type)
generalize e1 t1 = do
  (cx, _, _) <- get
  let cfv = Set.fromList $ Map.foldr (\t2 as -> freeTVars t2 ++ as) [] cx
      tfv = filter (`Set.notMember` cfv) $ nub $ freeTVars e1 ++ freeTVars t1
  return (foldr FETAbs e1 tfv, foldr TForAll t1 tfv)

-- Instantiate outer universal quantifiers with fresh type variables.
openUniversals :: FTerm -> Type -> TypeCheck (FTerm, Type)
openUniversals e (TForAll a1 t) = do
  a2 <- freshTVar (Just $ fromTVarName a1)
  openUniversals (FETApp e (TVar a2)) (subst a1 (TVar a2) t)
openUniversals e t = return (e, t)

-- Instantiate outer existential quantifiers with fresh type variables.
openExistentials :: Type -> TypeCheck Type
openExistentials (TExists a1 t) = do
  a2 <- freshTVar (Just $ fromTVarName a1)
  openExistentials (subst a1 (TVar a2) t)
openExistentials t = return t

-- Check that a given type satisfies the following criteria:
-- 1. All free type variables are bound in the initial context.
-- 2. All free type constants are bound in the initial context.
-- 3. The type contains no inner existential quantifiers.
-- 4. If outerExistentialsAllowed is False, the type contains no outer
--    existential quantifiers.
checkAnnotation :: Bool -> Type -> TypeCheck ()
checkAnnotation outerExistentialsAllowed t1 = do
  let fv = freeTVars t1
      fc = freeTConsts t1
      (_, ca, cc) = initialContext
  if Set.isSubsetOf (Set.fromList fv) ca
    then return ()
    else throwError $
         "Type annotation contains unbound type variables: " ++ show t1
  if Set.isSubsetOf (Set.fromList fc) cc
    then return ()
    else throwError $
         "Type annotation contains unbound type constants: " ++ show t1
  checkForExistentials $
    if outerExistentialsAllowed
      then let (BExists _, t2) = collectBinders t1
           in t2
      else t1
  where
    checkForExistentials :: Type -> TypeCheck ()
    checkForExistentials (TVar _) = return ()
    checkForExistentials (TConst _) = return ()
    checkForExistentials (TArrow t2 t3) = do
      checkForExistentials t2
      checkForExistentials t3
    checkForExistentials (TForAll _ t2) = checkForExistentials t2
    checkForExistentials (TExists a t2) =
      throwError $ "Unexpected existential: " ++ show (TExists a t2)

-- A helper method for type checking binary operations (e.g., arithmetic
-- operations).
checkBinary ::
     ITerm -> Type -> ITerm -> Type -> TypeCheck (FTerm, FTerm, Substitution)
checkBinary e1 t1 e2 t2 = do
  (e3, t3, theta1) <- infer e1
  (e4, theta2) <- subsume e3 t3 t1
  let theta3 = composeSubst theta1 theta2
  (e5, t4, theta4) <- infer e2
  let theta5 = composeSubst theta3 theta4
  (e6, theta6) <- subsume e5 t4 t2
  let theta7 = composeSubst theta5 theta6
  return (applySubst theta7 e4, e6, theta7)

-- Infer the type of a term. Inference may involve unification. This function
-- returns a substitution which is also applied to the context.
infer :: ITerm -> TypeCheck (FTerm, Type, Substitution)
infer (IEIntLit i) = return (FEIntLit i, intType, Map.empty)
infer (IEAddInt e1 e2) = do
  (e3, e4, theta) <- checkBinary e1 intType e2 intType
  (e5, t) <- generalize (FEAddInt e3 e4) intType
  return (e5, t, theta)
infer (IESubInt e1 e2) = do
  (e3, e4, theta) <- checkBinary e1 intType e2 intType
  (e5, t) <- generalize (FESubInt e3 e4) intType
  return (e5, t, theta)
infer (IEMulInt e1 e2) = do
  (e3, e4, theta) <- checkBinary e1 intType e2 intType
  (e5, t) <- generalize (FEMulInt e3 e4) intType
  return (e5, t, theta)
infer (IEDivInt e1 e2) = do
  (e3, e4, theta) <- checkBinary e1 intType e2 intType
  (e5, t) <- generalize (FEDivInt e3 e4) intType
  return (e5, t, theta)
infer (IEVar x) = do
  (cx, _, _) <- get
  case Map.lookup x cx of
    Just t -> return (FEVar x, t, Map.empty)
    Nothing -> throwError $ "Undefined variable: " ++ show x
infer (IEAbs x1 t1 e1) = do
  checkAnnotation True t1
  t2 <- openExistentials t1
  x2 <- freshEVar (Just $ fromEVarName x1) t2
  (e2, t3, theta) <- infer (subst x1 (IEVar x2) e1)
  deleteEVar x2
  let t4 = applySubst theta t2
  case (t2, t4) of
    (TForAll _ _, _) -> return ()
    (_, TForAll _ _) ->
      throwError $ "Inferred polymorphic argument type: " ++ show t4
    _ -> return ()
  (e3, t5) <- openUniversals e2 t3
  (e4, t6) <-
    generalize
      (FEAbs x2 (applySubst theta t2) (applySubst theta e3))
      (TArrow t4 (applySubst theta t5))
  return (e4, t6, theta)
infer (IEApp e1 e2) = do
  (e3, t1, theta1) <- infer e1
  a1 <- freshTVar Nothing
  a2 <- freshTVar Nothing
  (e4, theta2) <- subsume e3 t1 (TArrow (TVar a1) (TVar a2))
  let theta3 = composeSubst theta1 theta2
      t2 = applySubst theta3 (TVar a1)
      t3 = applySubst theta3 (TVar a2)
  (e5, t4, theta4) <- infer e2
  let theta5 = composeSubst theta3 theta4
  (e6, theta6) <- subsume e5 t4 (applySubst theta5 t2)
  let theta7 = composeSubst theta5 theta6
  (e7, t5) <-
    generalize (applySubst theta7 (FEApp e4 e6)) (applySubst theta7 t3)
  return (e7, t5, theta7)
infer (IELet x1 e1 e2) = do
  (e3, t1, theta1) <- infer e1
  x2 <- freshEVar (Just $ fromEVarName x1) t1
  (e4, t2, theta2) <- infer (subst x1 (IEVar x2) e2)
  deleteEVar x2
  return
    ( FEApp (FEAbs x2 (applySubst theta2 t1) e4) (applySubst theta2 e3)
    , t2
    , composeSubst theta1 theta2)
infer (IEAnno e1 t1) = do
  checkAnnotation False t1
  (e2, t2, theta1) <- infer e1
  (e3, theta2) <- subsume e2 t2 (applySubst theta1 t1)
  (e4, t3) <- generalize e3 t1
  return (e4, t3, composeSubst theta1 theta2)

-- Type inference can generate superfluous type abstractions and applications.
-- This function removes them.
simplify :: FTerm -> FTerm
simplify e@(FEIntLit _) = e
simplify (FEAddInt e1 e2) = FEAddInt (simplify e1) (simplify e2)
simplify (FESubInt e1 e2) = FESubInt (simplify e1) (simplify e2)
simplify (FEMulInt e1 e2) = FEMulInt (simplify e1) (simplify e2)
simplify (FEDivInt e1 e2) = FEDivInt (simplify e1) (simplify e2)
simplify e@(FEVar _) = e
simplify (FEAbs x t e) = FEAbs x t (simplify e)
simplify (FEApp e1 e2) = FEApp (simplify e1) (simplify e2)
simplify (FETAbs a1 (FETApp e (TVar a2)))
  | a1 == a2 && a1 `notElem` freeTVars e = e
simplify (FETAbs a e) = FETAbs a (simplify e)
simplify (FETApp (FETAbs a e) t) = simplify (subst a t e)
simplify (FETApp e t) = FETApp (simplify e) t

-- Given a term in the untyped language, return a term in the typed language
-- together with its type.
typeCheck :: ITerm -> Either String (FTerm, Type)
typeCheck e1 =
  (\(e2, t, _) -> (simplify e2, t)) <$>
  evalState (runExceptT (infer e1)) initialContext
