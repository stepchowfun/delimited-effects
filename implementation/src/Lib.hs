module Lib
  ( Context(..)
  , EffectMap(..)
  , Partial
  , Row(..)
  , Term(..)
  , Type(..)
  , abort
  , assert
  , checkTypeAndRow
  , checkTypeInferRow
  , contextLookup
  , effectMapLookup
  , inferTypeAndRow
  , inferTypeCheckRow
  , maybeToPartial
  , partialToMaybe
  , subrow
  , substituteEffectsInRow
  , substituteEffectsInType
  , subtype ) where

import Error
  ( Partial
  , abort
  , assert
  , maybeToPartial
  , partialToMaybe )
import Inference
  ( checkTypeAndRow
  , checkTypeInferRow
  , inferTypeAndRow
  , inferTypeCheckRow )
import Subrow (subrow)
import Subtype (subtype)
import Syntax
  ( Context(..)
  , EffectMap(..)
  , Row(..)
  , Term(..)
  , Type(..)
  , contextLookup
  , effectMapLookup
  , substituteEffectsInRow
  , substituteEffectsInType )
