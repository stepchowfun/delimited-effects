module Lib
  ( Context(..)
  , EffectMap(..)
  , Partial
  , Row(..)
  , Term(..)
  , Type(..)
  , VarSet(..)
  , abort
  , assert
  , check
  , contextLookup
  , effectMapLookup
  , effects
  , freeVars
  , infer
  , maybeToPartial
  , rowContains
  , subrow
  , substituteEffectInRow
  , substituteEffectInType
  , varSetContains ) where

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
  , Type(..)
  , VarSet(..)
  , contextLookup
  , effectMapLookup
  , effects
  , freeVars
  , rowContains
  , substituteEffectInRow
  , substituteEffectInType
  , varSetContains )
