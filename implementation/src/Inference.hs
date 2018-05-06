{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- The algorithm presented here is based on the paper by Daan Leijen called
-- "HMF: Simple type inference for first-class polymorphism". That paper was
-- published in The 13th ACM SIGPLAN International Conference on Functional
-- Programming (ICFP 2008).
module Inference
  ( typeCheck
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax
  ( BExists(..)
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
-- 1. The ability to read the context (via Reader).
-- 2. The ability to throw errors (via ExceptT).
type TypeCheck = ExceptT String (Reader Context)

-- Generate a fresh term variable.
withFreshEVar ::
     Maybe String -> Type -> (EVarName -> TypeCheck a) -> TypeCheck a
withFreshEVar x1 t k = do
  (cx, ca, cc) <- ask
  let names =
        ["x", "y", "z", "u", "v", "m", "n", "p", "q"] ++ ((++ "′") <$> names)
      x2 =
        EVarName $
        head $
        dropWhile
          (\x3 -> isJust $ Map.lookup (EVarName x3) cx)
          (case x1 of
             Just x4 -> x4 : names
             Nothing -> names)
  local (const (Map.insert x2 t cx, ca, cc)) $ k x2

-- Generate a fresh type variable.
withFreshTVar :: Maybe String -> (TVarName -> TypeCheck a) -> TypeCheck a
withFreshTVar a1 k = do
  (cx, ca, cc) <- ask
  let names =
        ["a", "b", "c", "d", "e", "f", "g", "h", "i"] ++ ((++ "′") <$> names)
      a2 =
        TVarName $
        head $
        dropWhile
          (\a3 -> Set.member (TVarName a3) ca)
          (case a1 of
             Just a4 -> a4 : names
             Nothing -> names)
  local (const (cx, Set.insert a2 ca, cc)) $ k a2

-- Generate a fresh type constant.
withFreshTConst :: Maybe String -> (TConstName -> TypeCheck a) -> TypeCheck a
withFreshTConst c1 k = do
  (cx, ca, cc) <- ask
  let names =
        ["A", "B", "C", "D", "E", "F", "G", "H", "I"] ++ ((++ "′") <$> names)
      c2 =
        TConstName $
        head $
        dropWhile
          (\c3 -> Set.member (TConstName c3) cc)
          (case c1 of
             Just c4 -> c4 : names
             Nothing -> names)
  local (const (cx, ca, Set.insert c2 cc)) $ k c2

-- A substitution maps type variables to types. We maintain the invariant that
-- all substitutions are idempotent.
type Substitution = Map TVarName Type

-- Compose two substitutions.
composeSubst :: Substitution -> Substitution -> Substitution
composeSubst theta1 theta2 =
  Map.union theta2 (Map.map (applySubst theta2) theta1)

-- Apply a substitution to a type.
class ApplySubst a where
  applySubst :: Substitution -> a -> a

instance ApplySubst FTerm where
  applySubst theta e = Map.foldrWithKey subst e theta

instance ApplySubst Type where
  applySubst theta t = Map.foldrWithKey subst t theta

-- Apply a substitution to a context.
withSubstContext :: Substitution -> TypeCheck a -> TypeCheck a
withSubstContext theta =
  local (\(cx, ca, cc) -> (Map.map (applySubst theta) cx, ca, cc))

-- Generalize a term and a type.
generalize :: FTerm -> Type -> TypeCheck (FTerm, Type)
generalize e1 t1 = do
  (_, ca, _) <- ask
  let fv =
        filter
          (\a -> not $ Set.member a ca)
          (nub $ freeTVars e1 ++ freeTVars t1)
  return $ foldr (\a (e2, t2) -> (FETAbs a e2, TForAll a t2)) (e1, t1) fv

-- Instantiate outer universal quantifiers with fresh type variables.
openUniversals ::
     FTerm -> Type -> (FTerm -> Type -> TypeCheck a) -> TypeCheck a
openUniversals e (TForAll a1 t) k =
  withFreshTVar (Just $ fromTVarName a1) $ \a2 ->
    openUniversals (FETApp e (TVar a2)) (subst a1 (TVar a2) t) k
openUniversals e t k = k e t

-- Instantiate outer existential quantifiers with fresh type variables.
openExistentials :: Type -> (Type -> TypeCheck a) -> TypeCheck a
openExistentials (TExists a1 t) k =
  withFreshTVar (Just $ fromTVarName a1) $ \a2 ->
    openExistentials (subst a1 (TVar a2) t) k
openExistentials t k = k t

-- Find a substitution that equates two types.
unify :: Type -> Type -> TypeCheck Substitution
unify (TVar a1) (TVar a2)
  | a1 == a2 = return Map.empty
unify (TVar a) t
  | a `notElem` freeTVars t = return $ Map.singleton a t
unify t (TVar a)
  | a `notElem` freeTVars t = return $ Map.singleton a t
unify (TConst c1) (TConst c2)
  | c1 == c2 = return Map.empty
unify (TArrow t1 t2) (TArrow t3 t4) = do
  theta1 <- unify t1 t3
  theta2 <- unify (applySubst theta1 t2) (applySubst theta1 t4)
  return $ composeSubst theta1 theta2
unify (TForAll a1 t1) (TForAll a2 t2) =
  withFreshTConst Nothing $ \c -> do
    theta <- unify (subst a1 (TConst c) t1) (subst a2 (TConst c) t2)
    if c `elem` Map.foldr (\t3 fc -> fc ++ freeTConsts t3) [] theta
      then throwError $
           "Unable to unify " ++
           show (TForAll a1 t1) ++ " with " ++ show (TForAll a2 t2)
      else return theta
unify (TExists a1 t1) (TExists a2 t2) =
  withFreshTConst Nothing $ \c -> do
    theta <- unify (subst a1 (TConst c) t1) (subst a2 (TConst c) t2)
    if c `elem` Map.foldr (\t3 fc -> fc ++ freeTConsts t3) [] theta
      then throwError $
           "Unable to unify " ++
           show (TExists a1 t1) ++ " with " ++ show (TExists a2 t2)
      else return theta
unify t1 t2 =
  throwError $ "Unable to unify " ++ show t1 ++ " with " ++ show t2

-- Compute the most general substitution that makes the second type a
-- generalization of the first. If underdetermined type applications are
-- necessary, this function will introduce fresh type variables into the
-- context to use as type arguments.
subsume ::
     Type
  -> FTerm
  -> Type
  -> (FTerm -> Substitution -> TypeCheck a)
  -> TypeCheck a
subsume (TForAll a1 t1) e1 t2 k =
  withFreshTConst (Just $ fromTVarName a1) $ \c ->
    subsume (subst a1 (TConst c) t1) e1 t2 $ \e2 theta ->
      if c `elem` Map.foldr (\t3 cs -> freeTConsts t3 ++ cs) [] theta
        then throwError $
             show (TForAll a1 t1) ++ " is not subsumed by " ++ show t2
        else withFreshTVar (Just $ fromTVarName a1) $ \a2 ->
               k (FETAbs a2 (subst c (TVar a2) e2)) theta
subsume t1 e1 (TForAll a1 t2) k =
  withFreshTVar (Just $ fromTVarName a1) $ \a2 ->
    subsume t1 (subst a1 (TVar a2) e1) (subst a1 (TVar a2) t2) $ \e2 theta ->
      case Map.lookup a2 theta of
        Just t3 -> k (FETApp e2 t3) (Map.delete a2 theta)
        Nothing ->
          withFreshTVar Nothing $ \a3 -> k (FETApp e2 (TVar a3)) theta
subsume t1 e t2 k = do
  theta <- unify t1 t2
  k e theta

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

-- Infer the type of a term. The returned substitution is guaranteed to be a
-- "monomorphic" substitution, meaning the types in the codomain do not have
-- any outer quantifiers.
infer :: ITerm -> TypeCheck (FTerm, Type, Substitution)
infer (IEIntLit i) = return (FEIntLit i, intType, Map.empty)
infer (IEAddInt e1 e2) = do
  (e3, t1, theta1) <- infer e1
  (e4, theta2) <-
    subsume intType e3 t1 $ \e4 theta2 -> do
      let theta3 = composeSubst theta1 theta2
      withSubstContext theta3 $ do
        (e5, t2, theta4) <- infer e2
        let theta5 = composeSubst theta3 theta4
        subsume intType e5 t2 $ \e6 theta6 -> do
          let theta7 = composeSubst theta5 theta6
          return (FEAddInt e4 e6, theta7)
  (e5, t2) <- generalize e4 intType
  return (e5, t2, composeSubst theta1 theta2)
infer (IESubInt e1 e2) = do
  (e3, t1, theta1) <- infer e1
  (e4, theta2) <-
    subsume intType e3 t1 $ \e4 theta2 -> do
      let theta3 = composeSubst theta1 theta2
      withSubstContext theta3 $ do
        (e5, t2, theta4) <- infer e2
        let theta5 = composeSubst theta3 theta4
        subsume intType e5 t2 $ \e6 theta6 -> do
          let theta7 = composeSubst theta5 theta6
          return (FESubInt e4 e6, theta7)
  (e5, t2) <- generalize e4 intType
  return (e5, t2, composeSubst theta1 theta2)
infer (IEMulInt e1 e2) = do
  (e3, t1, theta1) <- infer e1
  (e4, theta2) <-
    subsume intType e3 t1 $ \e4 theta2 -> do
      let theta3 = composeSubst theta1 theta2
      withSubstContext theta3 $ do
        (e5, t2, theta4) <- infer e2
        let theta5 = composeSubst theta3 theta4
        subsume intType e5 t2 $ \e6 theta6 -> do
          let theta7 = composeSubst theta5 theta6
          return (FEMulInt e4 e6, theta7)
  (e5, t2) <- generalize e4 intType
  return (e5, t2, composeSubst theta1 theta2)
infer (IEDivInt e1 e2) = do
  (e3, t1, theta1) <- infer e1
  (e4, theta2) <-
    subsume intType e3 t1 $ \e4 theta2 -> do
      let theta3 = composeSubst theta1 theta2
      withSubstContext theta3 $ do
        (e5, t2, theta4) <- infer e2
        let theta5 = composeSubst theta3 theta4
        subsume intType e5 t2 $ \e6 theta6 -> do
          let theta7 = composeSubst theta5 theta6
          return (FEDivInt e4 e6, theta7)
  (e5, t2) <- generalize e4 intType
  return (e5, t2, composeSubst theta1 theta2)
infer (IEVar x) = do
  (cx, _, _) <- ask
  case Map.lookup x cx of
    Just t -> return (FEVar x, t, Map.empty)
    Nothing -> throwError $ "Undefined variable: " ++ show x
infer (IEAbs x1 t1 e1) = do
  checkAnnotation True t1
  (e2, t2, theta) <-
    openExistentials t1 $ \t2 ->
      withFreshEVar (Just $ fromEVarName x1) t2 $ \x2 -> do
        (e2, t3, theta) <- infer (subst x1 (IEVar x2) e1)
        openUniversals e2 t3 $ \e3 t4 ->
          return
            ( FEAbs x2 (applySubst theta t2) (applySubst theta e3)
            , applySubst theta (TArrow t2 t4)
            , theta)
  (e3, t3) <- generalize e2 t2
  return (e3, t3, theta)
infer (IEApp e1 e2) = do
  (e3, t1, theta1) <- infer e1
  (e4, t2, theta2) <-
    withSubstContext theta1 $
    withFreshTVar Nothing $ \a1 ->
      withFreshTVar Nothing $ \a2 ->
        subsume (TArrow (TVar a1) (TVar a2)) e3 t1 $ \e4 theta2 -> do
          let theta3 = composeSubst theta1 theta2
              t2 = applySubst theta3 (TVar a1)
              t3 = applySubst theta3 (TVar a2)
          withSubstContext theta3 $ do
            (e5, t4, theta4) <- infer e2
            let theta5 = composeSubst theta3 theta4
            subsume (applySubst theta5 t2) e5 t4 $ \e6 theta6 -> do
              let theta7 = composeSubst theta5 theta6
                  (theta8, theta9) =
                    Map.partition
                      (\case
                         TForAll _ _ -> True
                         _ -> False)
                      theta6
              (_, ca, _) <- ask
              if Set.null $ Map.keysSet theta8 `Set.intersection` ca
                then return
                       ( applySubst theta7 (FEApp e4 e6)
                       , applySubst theta7 t3
                       , composeSubst theta5 theta9)
                else throwError $
                     "Unable to determine the type of " ++ show (IEApp e1 e2)
  (e5, t3) <- generalize e4 t2
  return (e5, t3, theta2)
infer (IELet x1 e1 e2) = do
  (e3, t1, theta1) <- infer e1
  withSubstContext theta1 $
    withFreshEVar (Just $ fromEVarName x1) t1 $ \x2 -> do
      (e4, t2, theta2) <- infer (subst x1 (IEVar x2) e2)
      return (FEApp (FEAbs x2 t1 e4) e3, t2, composeSubst theta1 theta2)
infer (IEAnno e1 t1) = do
  checkAnnotation False t1
  (e2, t2, theta1) <- infer e1
  (e3, theta2) <- subsume (applySubst theta1 t1) e2 t2 $ curry return
  (e4, t3) <- generalize e3 t1
  return (e4, t3, composeSubst theta1 theta2)

-- Given a term in the untyped language, return a term in the typed language
-- together with its type.
typeCheck :: ITerm -> Either String (FTerm, Type)
typeCheck e1 =
  (\(e2, t, _) -> (e2, t)) <$>
  runReader (runExceptT (infer e1)) initialContext
