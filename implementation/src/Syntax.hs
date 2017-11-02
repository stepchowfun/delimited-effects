module Syntax
  ( Term(..)
  , Type(..)
  , Row(..)
  , Context(..)
  , EffectMap(..) ) where

data Term a
  = EUnit
  | EVar a
  | EAbs a (Type a) (Term a)
  | EApp (Term a) (Term a)
  | EProvide a [a] (Term a) (Term a)

data Type a
  = TUnit
  | TArrow (Type a) (Type a) (Row a)

data Row a
  = REmpty
  | RSingleton a
  | RUnion (Row a) (Row a)
  | RDifference (Row a) (Row a)
  deriving (Eq, Show)

data Context a
  = CEmpty
  | CExtend (Context a) a (Type a) (Row a)

data EffectMap a
  = EMEmpty
  | EMExtend (EffectMap a) a a (Type a) (Row a)
