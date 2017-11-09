module Lib
  ( Context(..)
  , EffectMap(..)
  , Row(..)
  , Term(..)
  , Type(..)
  , checkTypeAndRow
  , checkTypeInferRow
  , contextLookup
  , effectMapLookup
  , inferTypeAndRow
  , inferTypeCheckRow
  , subrow
  , substituteEffectsInRow
  , substituteEffectsInType
  , subtype ) where

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
