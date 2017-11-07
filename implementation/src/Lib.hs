module Lib (
    Term(..)
  , Context(..)
  , EffectMap(..)
  , Row(..)
  , Type(..)
  , contextLookup
  , effectMapLookup
  , subrow
  , subtype ) where

import Subrow (subrow)
import Subtype (subtype)
import Syntax (
    Term(..)
  , Context(..)
  , EffectMap(..)
  , Row(..)
  , Type(..)
  , contextLookup
  , effectMapLookup )
