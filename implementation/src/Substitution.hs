{-# LANGUAGE FunctionalDependencies #-}

module Substitution
  ( ApplySubst
  , Substitution
  , SubstRemoveKeys
  , applySubst
  , composeSubst
  , emptySubst
  , singletonSubst
  , substRemoveKeys
  ) where

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax
  ( FTerm(..)
  , FreeKVars
  , FreeTCons
  , FreeTVars
  , KVarName(..)
  , Kind(..)
  , TVarName(..)
  , Type(..)
  , freeKVars
  , freeTCons
  , freeTVars
  , subst
  )

-- A substitution maps type variables to types and kind variables to kinds. We
-- maintain the invariant that all substitutions are idempotent.
data Substitution =
  Substitution (Map TVarName Type)
               (Map KVarName Kind)

-- The free type variables of the codomain of a substitution
instance FreeTVars Substitution where
  freeTVars (Substitution m n) =
    nub $
    Map.foldr (\t as -> freeTVars t ++ as) [] m ++
    Map.foldr (\k as -> freeTVars k ++ as) [] n

-- The free type constructors of the codomain of a substitution
instance FreeTCons Substitution where
  freeTCons (Substitution m n) =
    nub $
    Map.foldr (\t cs -> freeTCons t ++ cs) [] m ++
    Map.foldr (\k cs -> freeTCons k ++ cs) [] n

-- The free kind variables of the codomain of a substitution
instance FreeKVars Substitution where
  freeKVars (Substitution m n) =
    nub $
    Map.foldr (\t bs -> freeKVars t ++ bs) [] m ++
    Map.foldr (\k bs -> freeKVars k ++ bs) [] n

-- Check that a substitution is idempotent.
idempotencyCheck :: Substitution -> Substitution
idempotencyCheck theta@(Substitution m n) =
  if Set.null (Map.keysSet m `Set.intersection` Set.fromList (freeTVars theta)) &&
     Set.null (Map.keysSet n `Set.intersection` Set.fromList (freeKVars theta))
    then theta
    else error $ "Non-idempotent substitution: " ++ show m ++ " " ++ show n

-- Construct an empty substitution.
emptySubst :: Substitution
emptySubst = Substitution Map.empty Map.empty

-- Construct a substitution containing a single element.
class SingletonSubst a b | a -> b where
  singletonSubst :: a -> b -> Substitution

instance SingletonSubst TVarName Type where
  singletonSubst a t =
    idempotencyCheck $ Substitution (Map.singleton a t) Map.empty

instance SingletonSubst KVarName Kind where
  singletonSubst b k =
    idempotencyCheck $ Substitution Map.empty (Map.singleton b k)

-- Compose two substitutions. The substitutions are in diagrammatic order, that
-- is, the second substitution comes after the first.
composeSubst :: Substitution -> Substitution -> Substitution
composeSubst (Substitution m1 n1) theta@(Substitution m2 n2) =
  idempotencyCheck $
  Substitution
    (Map.union m2 (Map.map (applySubst theta) m1))
    (Map.union n2 (Map.map (applySubst theta) n1))

-- Remove keys from a substitution.
class SubstRemoveKeys a where
  substRemoveKeys :: Set a -> Substitution -> Substitution

instance SubstRemoveKeys TVarName where
  substRemoveKeys as (Substitution m n) = Substitution (Map.withoutKeys m as) n

instance SubstRemoveKeys KVarName where
  substRemoveKeys bs (Substitution m n) = Substitution m (Map.withoutKeys n bs)

-- Substitutions can be applied to various entities.
class ApplySubst a where
  applySubst :: Substitution -> a -> a

-- We deliberately omit an ApplySubst ITerm instance. The only free type or
-- kind variables of an ITerm come from type annotations, but free type
-- variables in annotations are interpreted as implicitly existentially bound
-- (i.e., they aren't really free).
instance ApplySubst FTerm where
  applySubst (Substitution m n) e =
    Map.foldrWithKey subst (Map.foldrWithKey subst e m) n

instance ApplySubst Type where
  applySubst (Substitution m n) t =
    Map.foldrWithKey subst (Map.foldrWithKey subst t m) n

instance ApplySubst Kind where
  applySubst (Substitution m n) t =
    Map.foldrWithKey subst (Map.foldrWithKey subst t m) n
