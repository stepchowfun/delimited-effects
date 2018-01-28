module Lib
  ( Context(..)
  , EffectMap(..)
  , Partial
  , Row(..)
  , Term(..)
  , TermVar(..)
  , Type(..)
  , TypeVar(..)
  , abort
  , assert
  , check
  , contextLookupKind
  , contextLookupType
  , effectMapLookup
  , infer
  , maybeToPartial
  , rowContains
  , subrow
  , substituteEffectInRow
  , substituteEffectInType
  , substituteTypeInType
  , typeVars ) where

import Error
  ( Partial
  , abort
  , assert
  , maybeToPartial )
import Inference (check, infer)
import Subrow (subrow)
import Syntax
  ( Context(..)
  , EffectMap(..)
  , Row(..)
  , Term(..)
  , TermVar(..)
  , Type(..)
  , TypeVar(..)
  , contextLookupKind
  , contextLookupType
  , effectMapLookup
  , rowContains
  , substituteEffectInRow
  , substituteEffectInType
  , substituteTypeInType
  , typeVars )
