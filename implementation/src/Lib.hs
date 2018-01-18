module Lib
  ( Context(..)
  , EffectMap(..)
  , HoistedSet(..)
  , Partial
  , Row(..)
  , Term(..)
  , Type(..)
  , abort
  , assert
  , check
  , contextLookupKind
  , contextLookupType
  , eff
  , effectMapLookup
  , effects
  , ftv
  , fv
  , infer
  , maybeToPartial
  , rowContains
  , subrow
  , substituteEffectInRow
  , substituteEffectInType
  , substituteTypeInType
  ) where

import Error (Partial, abort, assert, maybeToPartial)
import Inference (check, infer)
import Subrow (subrow)
import Syntax
       (Context(..), EffectMap(..), HoistedSet(..), Row(..), Term(..),
        Type(..), contextLookupKind, contextLookupType, eff,
        effectMapLookup, effects, ftv, fv, rowContains,
        substituteEffectInRow, substituteEffectInType,
        substituteTypeInType)
