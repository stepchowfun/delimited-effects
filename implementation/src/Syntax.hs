module Syntax
  ( Context(..)
  , Term(..)
  , Type(..) ) where

-- Data types

data Term -- Metavariable: e
  = EVar String
  | EAbs String Term
  | EApp Term Term
  | ETAbs String Term
  | ETApp Term Type
  | EAnno Term Type
  deriving (Eq, Show)

data Type -- Metavariable: t
  = TVar String
  | TArrow Type Type
  | TForAll String Type
  deriving (Eq, Show)

data Context -- Metavariable: c
  = CEmpty
  | CTExtend Context String Type
  | CKExtend Context String
  deriving (Eq, Show)
