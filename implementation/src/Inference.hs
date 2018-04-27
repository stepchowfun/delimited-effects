{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Inference
  ( typeCheck
  ) where

import Control.Arrow (first, second)
import Control.Monad (foldM, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.State (StateT, get, put, runStateT)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax
  ( CollectParams
  , EVar(..)
  , FTerm(..)
  , ITerm(..)
  , TVar(..)
  , Type(..)
  , collectParams
  , presentParams
  , substEVarInTerm
  , substTVarInFTerm
  , tFreeVars
  )

-- Unification variables are holes in a type.
newtype Unifier = ToUnifier
  { fromUnifier :: String
  } deriving (Eq, Ord)

instance Show Unifier where
  show = fromUnifier

-- A PartialType is like a Type but it may contain unification variables.
data PartialType
  = PTUnifier Unifier
  | PTVar TVar
  | PTArrow PartialType
            PartialType
  | PTForAll TVar
             PartialType

ptFreeVars :: PartialType -> [TVar]
ptFreeVars (PTUnifier _) = []
ptFreeVars (PTVar a) = [a]
ptFreeVars (PTArrow t1 t2) = ptFreeVars t1 ++ ptFreeVars t2
ptFreeVars (PTForAll a t) = filter (/= a) (ptFreeVars t)

instance CollectParams PartialType String where
  collectParams (PTUnifier u) = ([], PTUnifier u)
  collectParams (PTVar a) = ([], PTVar a)
  collectParams (PTArrow t1 t2) = ([], PTArrow t1 t2)
  collectParams (PTForAll a t1) =
    let (as, t2) = collectParams t1
    in (show a : as, t2)

instance Eq PartialType where
  PTUnifier u1 == PTUnifier u2 = u1 == u2
  PTVar a1 == PTVar a2 = a1 == a2
  PTArrow t1 t2 == PTArrow t3 t4 = t1 == t3 && t2 == t4
  PTForAll a1 t1 == PTForAll a2 t2 =
    t1 == substVarInPartialType a2 (PTVar a1) t2 &&
    t2 == substVarInPartialType a1 (PTVar a2) t1
  _ == _ = False

instance Show PartialType where
  show (PTUnifier u) = fromUnifier u ++ "?"
  show (PTVar a) = show a
  show (PTArrow (PTVar a) t) = show a ++ " -> " ++ show t
  show (PTArrow t1 t2) = "(" ++ show t1 ++ ") -> " ++ show t2
  show (PTForAll a t1) =
    let (as, t2) = collectParams (PTForAll a t1)
    in "∀" ++ presentParams as ++ " . " ++ show t2

-- Substitution
substVarInPartialType :: TVar -> PartialType -> PartialType -> PartialType
substVarInPartialType _ _ (PTUnifier u) = PTUnifier u
substVarInPartialType a1 t (PTVar a2) =
  if a1 == a2
    then t
    else PTVar a2
substVarInPartialType a t1 (PTArrow t2 t3) =
  PTArrow (substVarInPartialType a t1 t2) (substVarInPartialType a t1 t3)
substVarInPartialType a1 t1 (PTForAll a2 t2) =
  PTForAll a2 $
  if a1 == a2
    then t2
    else substVarInPartialType a1 t1 t2

substUnifierInPartialType ::
     Unifier -> PartialType -> PartialType -> PartialType
substUnifierInPartialType u1 t (PTUnifier u2) =
  if u1 == u2
    then t
    else PTUnifier u2
substUnifierInPartialType _ _ (PTVar a) = PTVar a
substUnifierInPartialType u t1 (PTArrow t2 t3) =
  PTArrow
    (substUnifierInPartialType u t1 t2)
    (substUnifierInPartialType u t1 t3)
substUnifierInPartialType u t1 (PTForAll a t2) =
  PTForAll a (substUnifierInPartialType u t1 t2)

-- Contexts can hold term variables and type variables.
type Context = (Map EVar Type, Set TVar)

-- The TypeCheck monad provides:
-- 1. The ability to read the context (via Reader).
-- 2. The ability to generate fresh variables (via StateT).
-- 3. The ability to throw errors (via ExceptT).
type TypeCheck = ExceptT String (StateT Int (Reader Context))

-- Generate a fresh term variable.
freshEVar :: String -> TypeCheck EVar
freshEVar x = do
  context <- ask
  case Map.lookup (ToEVar x) (fst context) of
    Just _ -> do
      i <- get
      put (i + 1)
      return $ ToEVar ("@" ++ show i)
    Nothing -> return $ ToEVar x

-- Generate a fresh type variable.
freshTVar :: String -> TypeCheck TVar
freshTVar a = do
  context <- ask
  if Set.member (ToTVar a) (snd context)
    then do
      i <- get
      put (i + 1)
      return $ ToTVar ("@" ++ show i)
    else return $ ToTVar a

-- Generate a fresh unifier.
freshUnifier :: String -> TypeCheck Unifier
freshUnifier u = return $ ToUnifier (u ++ "?")

-- Look up a term variable in the context.
eLookupVar :: EVar -> TypeCheck Type
eLookupVar x = do
  context <- ask
  case Map.lookup x (fst context) of
    Just t -> return t
    Nothing -> throwError $ "Undefined term variable: " ++ show x

-- Check that a type variable is in the context.
tLookupVar :: TVar -> TypeCheck ()
tLookupVar a = do
  context <- ask
  if Set.member a (snd context)
    then return ()
    else throwError $ "Undefined type variable: " ++ show a

-- Convert a Type to a PartialType.
makePartial :: Type -> PartialType
makePartial (TVar a) = PTVar a
makePartial (TArrow t1 t2) = PTArrow (makePartial t1) (makePartial t2)
makePartial (TForAll a t) = PTForAll a (makePartial t)

-- Convert a PartialType to a Type. An error will be thrown if the type
-- contains any unifiers.
makeTotal :: PartialType -> TypeCheck Type
makeTotal (PTUnifier u) = throwError $ "Unexpected unifier: " ++ show u
makeTotal (PTVar a) = return $ TVar a
makeTotal (PTArrow t1 t2) = do
  t3 <- makeTotal t1
  t4 <- makeTotal t2
  return $ TArrow t3 t4
makeTotal (PTForAll a t1) = do
  t2 <- makeTotal t1
  return $ TForAll a t2

-- Instantiate outer quantifiers with fresh unifiers. Returns the type
-- and a map from fresh unifiers to eliminated type variables.
open :: PartialType -> TypeCheck (PartialType, [(Unifier, TVar)])
open (PTUnifier u) = return (PTUnifier u, [])
open (PTVar a) = return (PTVar a, [])
open (PTArrow t1 t2) = return (PTArrow t1 t2, [])
open (PTForAll a t1) = do
  u <- freshUnifier (fromTVar a)
  (t2, us) <- open (substVarInPartialType a (PTUnifier u) t1)
  return (t2, (u, a) : us)

-- This is needed when we need to eliminate type abstractions and we don't
-- care what type we use.
unitType :: Type
unitType = TForAll (ToTVar "a") (TVar $ ToTVar "a")

-- This function applies a term and a type, doing beta reduction if possible.
applyType :: FTerm -> Type -> FTerm
applyType (FETAbs a e) t = substTVarInFTerm a t e
applyType e t = FETApp e t

-- Find an instantiation that turns the second PartialType into the first.
unify :: PartialType -> PartialType -> TypeCheck (Map Unifier Type)
unify t1 (PTUnifier u) = do
  t2 <- makeTotal t1
  return $ Map.singleton u t2
unify (PTVar a1) (PTVar a2) =
  if a1 == a2
    then return Map.empty
    else throwError $ "Unable to unify " ++ show a1 ++ " with " ++ show a2
unify (PTArrow t1 t2) (PTArrow t3 t4) = do
  is1 <- unify t1 t3
  is2 <- unify t2 t4
  if all
       (\u -> Map.lookup u is1 == Map.lookup u is2)
       (Map.keys (Map.intersection is1 is2))
    then return ()
    else throwError $
         "Unable to unify " ++
         show (PTArrow t1 t2) ++ " with " ++ show (PTArrow t3 t4)
  return $ Map.union is1 is2
unify (PTForAll a1 t1) (PTForAll a2 t2) = do
  when (a1 `elem` ptFreeVars (PTForAll a2 t2)) $
    throwError $
    "Unable to unify " ++
    show (PTForAll a1 t1) ++ " with " ++ show (PTForAll a2 t2)
  unify t1 (substVarInPartialType a2 (PTVar a1) t2)
unify t1 t2 =
  throwError $ "Unable to unify " ++ show t1 ++ " with " ++ show t2

-- Infer the type of a term.
infer :: ITerm -> TypeCheck (FTerm, Type)
infer (IEVar x) = do
  t <- eLookupVar x
  return (FEVar x, t)
infer (IEApp e1 e2)
  -- Infer the type of e1.
 = do
  (e3, t1) <- infer e1
  -- Instantiate all the outer quantifiers with fresh unifiers.
  (t2, usAndAs) <- open (makePartial t1)
  -- Get the argument and return types.
  (t3, t4) <-
    case t2 of
      PTArrow t3 t4 -> return (t3, t4)
      _ -> throwError $ "Not a function type: " ++ show t2
  -- Check the argument.
  (e4, uToT) <- check e2 t3
  -- Wrap the applicand in type applications and substitute away the unifiers
  -- in the return type.
  (e5, t5, as) <-
    foldM
      (\(e5, t5, as) (u, a1) -> do
         (t6, newAs) <-
           case Map.lookup u uToT of
             Just t6 -> return (t6, as)
             Nothing -> do
               a2 <- freshTVar (fromTVar a1)
               return (TVar a2, a2 : as)
         return
           ( applyType e5 t6
           , substUnifierInPartialType u (makePartial t6) t5
           , newAs))
      (e3, t4, [])
      usAndAs
  t6 <- makeTotal t5
  -- Construct the application
  let e6 = FEApp e5 e4
  -- Re-generalize the result.
  return $ foldl' (\(e7, t7) a -> (FETAbs a e7, TForAll a t7)) (e6, t6) as
infer (IEAnno e1 t) = do
  mapM_ tLookupVar (tFreeVars t)
  (e2, _) <- check e1 (makePartial t)
  return (e2, t)
infer t = throwError $ "Unable to infer the type of: " ++ show t

-- Check a term against a type and possibly instantiate unifiers in the type.
check :: ITerm -> PartialType -> TypeCheck (FTerm, Map Unifier Type)
check e1 (PTUnifier u) = do
  (e2, t) <- infer e1
  return (e2, Map.singleton u t)
check e1 (PTForAll a1 t) = do
  a2 <- freshTVar (fromTVar a1)
  (e2, uToT) <-
    local (second (Set.insert a2)) $
    check e1 (substVarInPartialType a1 (PTVar a2) t)
  return (FETAbs a2 e2, uToT)
check (IEAbs x1 e1) (PTArrow t1 t2) = do
  x2 <- freshEVar (fromEVar x1)
  t3 <- makeTotal t1
  (e2, uToT) <-
    local (first (Map.insert x2 t3)) $
    check (substEVarInTerm x1 (IEVar x2) e1) t2
  return (FEAbs x2 t3 e2, uToT)
check e1 t1
  -- Infer the type of e1.
 = do
  (e2, t2) <- infer e1
  -- Instantiate all the outer quantifiers with fresh unifiers.
  (t3, usAndAs) <- open (makePartial t2)
  -- Unify the result with t1.
  uToT1 <- unify t1 t3
  -- Wrap the term in type applications and substitute away the unifiers in the
  -- type.
  let (e4, t6) =
        foldl'
          (\(e3, t4) (u, _) ->
             let t5 = fromMaybe unitType (Map.lookup u uToT1)
             in ( applyType e3 t5
                , substUnifierInPartialType u (makePartial t5) t4))
          (e2, t3)
          usAndAs
  -- Unify the original type against the inferred type with eliminated
  -- quantifiers.
  uToT2 <- unify t6 t1
  return (e4, uToT2)

-- Given a term in the untyped language, return a term in the typed language
-- together with its type.
typeCheck :: ITerm -> Either String (FTerm, Type)
typeCheck e =
  let (result, _) =
        runReader (runStateT (runExceptT (infer e)) 0) (Map.empty, Set.empty)
  in result
