-- The algorithm presented here is based on the paper by Daan Leijen called
-- "HMF: Simple type inference for first-class polymorphism". That paper was
-- published in The 13th ACM SIGPLAN International Conference on Functional
-- Programming (ICFP 2008).
module Inference
  ( typeCheck
  ) where

import Control.Monad (foldM, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT, evalStateT, get, put)
import Data.Functor.Identity (Identity, runIdentity)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Substitution
  ( ApplySubst
  , Substitution
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
-- 2. The ability to read and update the context (also via State)
-- 3. The ability to accumulate a substitution and recall it (also via State)
-- 4. The ability to throw errors (via ExceptT)
type TypeCheck
   = StateT ( Integer
            , Map EVarName Type
            , Map TVarName Kind
            , Map TConName Kind
            , Substitution) (ExceptT String Identity)

-- Add a term variable to the context.
withUserEVar :: EVarName -> Type -> TypeCheck a -> TypeCheck a
withUserEVar x t k = do
  (i1, cx1, ca1, cc1, theta1) <- get
  when (Map.member x cx1) $ throwError $ "Variable already exists: " ++ show x
  put (i1, Map.insert x t cx1, ca1, cc1, theta1)
  result <- k
  (i2, cx2, ca2, cc2, theta2) <- get
  put (i2, Map.delete x cx2, ca2, cc2, theta2)
  pure result

-- Generate a fresh type variable and add it to the context.
freshTVar :: Kind -> TypeCheck TVarName
freshTVar k = do
  (i, cx, ca, cc, theta) <- get
  let a = AutoTVarName i
  put (i + 1, cx, Map.insert a k ca, cc, theta)
  pure a

-- Generate a fresh type constructor and add it to the context.
freshTCon :: Kind -> TypeCheck TConName
freshTCon k = do
  (i, cx, ca, cc, theta) <- get
  let c = AutoTConName i
  put (i + 1, cx, ca, Map.insert c k cc, theta)
  pure c

-- Generate a fresh type variable and add it to the context.
freshKVar :: TypeCheck KVarName
freshKVar = do
  (i, cx, ca, cc, theta) <- get
  let b = AutoKVarName i
  put (i + 1, cx, ca, cc, theta)
  pure b

-- Compose a substitution with the accumulator.
addSubst :: Substitution -> TypeCheck ()
addSubst theta1 = do
  (i, cx, ca, cc, theta2) <- get
  put
    ( i
    , Map.map (applySubst theta1) cx
    , Map.map (applySubst theta1) ca
    , Map.map (applySubst theta1) cc
    , composeSubst theta2 theta1)

-- Use the accumulated substitution.
runSubst :: ApplySubst a => a -> TypeCheck a
runSubst x = do
  (_, _, _, _, theta) <- get
  pure (applySubst theta x)

-- Compute the most general unifier and apply it to the context.
class Unify a where
  unify :: a -> a -> TypeCheck ()

instance Unify Kind where
  unify (KVar b1) (KVar b2)
    | b1 == b2 = pure ()
  unify (KVar b) k
    | b `notElem` freeKVars k = addSubst (singletonSubst b k)
  unify k (KVar b)
    | b `notElem` freeKVars k = addSubst (singletonSubst b k)
  unify KType KType = pure ()
  unify k1 k2 =
    throwError $ "Unable to unify " ++ show k1 ++ " with " ++ show k2

instance Unify Type where
  unify (TVar a1) (TVar a2)
    | a1 == a2 = pure ()
  unify (TVar a) t
    | a `notElem` freeTVars t = addSubst (singletonSubst a t)
  unify t (TVar a)
    | a `notElem` freeTVars t = addSubst (singletonSubst a t)
  unify t1@(TCon c1 ts1) t2@(TCon c2 ts2)
    | c1 == c2 = do
      if length ts1 == length ts2
        then pure ()
        else throwError $ "Unable to unify " ++ show t1 ++ " with " ++ show t2
      mapM_
        (\(t3, t4) -> do
           t5 <- runSubst t3
           t6 <- runSubst t4
           unify t5 t6)
        (zip ts1 ts2)
  unify t3@(TForAll a1 k1 t1) t4@(TForAll a2 k2 t2) = do
    c <- freshTCon k1
    t5 <- runSubst (subst a1 (TCon c []) t1)
    t6 <- runSubst (subst a2 (TCon c []) t2)
    unify k1 k2
    t7 <- runSubst t5
    t8 <- runSubst t6
    unify t7 t8
    (_, _, _, _, theta) <- get
    when (c `elem` freeTCons theta) $
      throwError $ "Unable to unify " ++ show t3 ++ " with " ++ show t4
  unify t1 t2 =
    throwError $ "Unable to unify " ++ show t1 ++ " with " ++ show t2

-- Instantiate, generalize, and unify as necessary to make a given term and
-- type match another given type.
subsume :: FTerm -> Type -> Type -> TypeCheck FTerm
subsume e1 t1 t2 = do
  let (BForAll aks1, t3) = collectBinders t1
      (BForAll aks2, t4) = collectBinders t2
  aks3 <-
    mapM
      (\(_, k) -> do
         a <- freshTVar k
         pure (a, k))
      aks1
  cks1 <-
    mapM
      (\(_, k) -> do
         c <- freshTCon k
         pure (c, k))
      aks2
  let e3 = foldr (\(a, _) e2 -> FETApp e2 (TVar a)) e1 aks3
      t5 = foldr (\((a1, _), (a2, _)) -> subst a1 (TVar a2)) t3 (zip aks1 aks3)
      t6 = foldr (\((a, _), (c, _)) -> subst a (TCon c [])) t4 (zip aks2 cks1)
  unify t5 t6
  (_, _, _, _, theta1) <- get
  let theta2 = substRemoveKeys (Set.fromList $ fst <$> aks3) theta1
  e4 <- runSubst e3
  if Set.null $
     Set.intersection
       (Set.fromList (fst <$> cks1))
       (Set.fromList $ freeTCons theta2)
    then pure ()
    else throwError $ show t2 ++ " is not subsumed by " ++ show t1
  caks <-
    mapM
      (\(c, k) -> do
         a <- freshTVar k
         pure (c, a, k))
      cks1
  pure $ foldr (\(c, a, k) e5 -> FETAbs a k (subst c (TVar a) e5)) e4 caks

-- Generalize a term and a type.
generalize :: FTerm -> Type -> TypeCheck (FTerm, Type)
generalize e1 t1 = do
  (_, cx, ca, _, _) <- get
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
  pure
    ( foldr (\(a, k) e2 -> FETAbs a k e2) e1 aks
    , foldr (\(a, k) t2 -> TForAll a k t2) t1 aks)

-- Instantiate outer universal quantifiers with fresh type variables.
open :: FTerm -> Type -> TypeCheck (FTerm, Type)
open e (TForAll a1 k t) = do
  a2 <- freshTVar k
  open (FETApp e (TVar a2)) (subst a1 (TVar a2) t)
open e t = pure (e, t)

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
         pure (subst a1 (TVar a2) t2))
      t1
      as
  t3 <-
    foldM
      (\t3 b1 -> do
         b2 <- freshKVar
         pure (subst b1 (KVar b2) t3))
      t2
      bs
  t4 <- replaceBoundVars t3
  (t5, _) <- infer t4
  pure t5
  where
    replaceBoundVars t2@(TVar _) = pure t2
    replaceBoundVars (TCon c ts1) = do
      ts2 <- mapM replaceBoundVars ts1
      pure $ TCon c ts2
    replaceBoundVars (TForAll a1 k t2) = do
      a2 <- freshTVar k
      t3 <- replaceBoundVars (subst a1 (TVar a2) t2)
      pure $ TForAll a2 k t3

-- Check the arities of type constructors.
class CheckArities a where
  checkArities :: a -> TypeCheck ()

instance CheckArities Type where
  checkArities (TVar _) = pure ()
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
        then pure ()
        else throwError $
             "The " ++
             show c ++
             " type constructor takes no type arguments, " ++
             "but was given " ++ show (length ts)
  checkArities (TCon c ts)
    | c == intName =
      if null ts
        then pure ()
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
  checkArities (TCon c _) = throwError $ "Unknown type constructor: " ++ show c
  checkArities (TForAll _ k t) = do
    checkArities k
    checkArities t
    pure ()

instance CheckArities Kind where
  checkArities (KVar _) = pure ()
  checkArities KType = pure ()

-- A type class for type checking (not inference).
class Check a b c d | a -> b c d where
  check :: a -> b -> TypeCheck (c, d)

instance Check ITerm Type FTerm Type where
  check e1 t1 = do
    (e2, t2) <- infer e1
    t3 <- runSubst t1
    e3 <- subsume e2 t2 t3
    pure (e3, t3)

instance Check (ITerm, ITerm) (Type, Type) (FTerm, FTerm) (Type, Type) where
  check (e1, e2) (t1, t2) = do
    (e3, t3) <- check e1 t1
    t4 <- runSubst t2
    (e4, t5) <- check e2 t4
    e5 <- runSubst e3
    t6 <- runSubst t3
    pure ((e5, e4), (t6, t5))

instance Check Type Kind Type Kind where
  check t1 k1 = do
    (t2, k2) <- infer t1
    unify k1 k2
    t3 <- runSubst t2
    k3 <- runSubst k1
    pure (t3, k3)

-- A type class for type inference.
class Infer a b c | a -> b c where
  infer :: a -> TypeCheck (b, c)

instance Infer Type Type Kind where
  infer (TVar a) = do
    (_, _, ca, _, _) <- get
    case Map.lookup a ca of
      Just k -> pure (TVar a, k)
      Nothing -> throwError $ "Undefined type variable: " ++ show a
  infer (TCon c ts1) = do
    ts2 <-
      foldM
        (\ts2 t1 -> do
           t2 <- runSubst t1
           (t3, _) <- check t2 KType
           ts3 <- mapM runSubst ts2
           pure (t3 : ts3))
        []
        ts1
    pure (TCon c (reverse ts2), KType)
  infer (TForAll a1 k1 t1) = do
    a2 <- freshTVar k1
    (t2, _) <- check (subst a1 (TVar a2) t1) KType
    k2 <- runSubst k1
    pure (TForAll a2 k2 t2, KType)

instance Infer ITerm FTerm Type where
  infer (IEVar x) = do
    (_, cx, _, _, _) <- get
    case Map.lookup x cx of
      Just t -> pure (FEVar x, t)
      Nothing -> throwError $ "Undefined variable: " ++ show x
  infer (IEAbs x t1 e1) = do
    t2 <- sanitizeAnnotation t1
    checkArities t2
    (e2, t3) <-
      withUserEVar x t2 $ do
        (e2, t3) <- infer e1
        t4 <- runSubst t2
        (t5, _) <- check t4 KType
        t6 <- runSubst t3
        e3 <- runSubst e2
        (e4, t7) <- open e3 t6
        pure (FEAbs x t5 e4, arrowType t5 t7)
    generalize e2 t3
  infer (IEApp e1 e2) = do
    a1 <- freshTVar KType
    a2 <- freshTVar KType
    (e3, TCon _ [t1, t2]) <- check e1 $ arrowType (TVar a1) (TVar a2)
    (e4, _) <- check e2 t1
    e5 <- runSubst e3
    t4 <- runSubst t2
    generalize (FEApp e5 e4) t4
  infer (IELet x e1 e2) = do
    (e3, t1) <- infer e1
    withUserEVar x t1 $ do
      (e4, t2) <- infer e2
      e5 <- runSubst e3
      t3 <- runSubst t1
      pure (FEApp (FEAbs x t3 e4) e5, t2)
  infer (IEAnno e1 t1) = do
    t2 <- sanitizeAnnotation t1
    checkArities t2
    (e2, t3) <- check e1 t2
    generalize e2 t3
  infer IETrue = pure (FETrue, boolType)
  infer IEFalse = pure (FEFalse, boolType)
  infer (IEIf e1 e2 e3) = do
    (e4, _) <- check e1 boolType
    t1 <- TVar <$> freshTVar KType
    ((e5, e6), (t2, _)) <- check (e2, e3) (t1, t1)
    e7 <- runSubst e4
    generalize (FEIf e7 e5 e6) t2
  infer (IEIntLit i) = pure (FEIntLit i, intType)
  infer (IEAdd e1 e2) = do
    ((e3, e4), _) <- check (e1, e2) (intType, intType)
    generalize (FEAdd e3 e4) intType
  infer (IESub e1 e2) = do
    ((e3, e4), _) <- check (e1, e2) (intType, intType)
    generalize (FESub e3 e4) intType
  infer (IEMul e1 e2) = do
    ((e3, e4), _) <- check (e1, e2) (intType, intType)
    generalize (FEMul e3 e4) intType
  infer (IEDiv e1 e2) = do
    ((e3, e4), _) <- check (e1, e2) (intType, intType)
    generalize (FEDiv e3 e4) intType
  infer (IEList es1) = do
    t1 <- TVar <$> freshTVar KType
    es2 <-
      foldM
        (\es2 e1 -> do
           t2 <- runSubst t1
           (e2, _) <- check e1 t2
           es3 <- mapM runSubst es2
           pure (e2 : es3))
        []
        es1
    t2 <- runSubst t1
    generalize (FEList $ reverse es2) (listType t2)
  infer (IEConcat e1 e2) = do
    t1 <- listType . TVar <$> freshTVar KType
    ((e3, e4), (t2, _)) <- check (e1, e2) (t1, t1)
    generalize (FEConcat e3 e4) t2

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
        runIdentity $
        runExceptT $
        evalStateT
          (infer e1)
          ( 0
          , Map.empty
          , Map.empty
          , Map.fromList
              [ (boolName, KType)
              , (intName, KType)
              , (listName, KType)
              , (arrowName, KType)
              ]
          , emptySubst)
   in case result of
        Left s -> Left s
        Right (e2, t) -> Right (simplify e2, t)
