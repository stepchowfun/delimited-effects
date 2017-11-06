module Types (Effect(..), Variable(..), effects, variables) where

import Test.QuickCheck
  ( Arbitrary
  , arbitrary
  , elements )

data Effect = EffectA | EffectB | EffectC | EffectD | EffectE
  deriving (Eq, Ord, Show)

instance Arbitrary Effect where
  arbitrary = elements effects

effects :: [Effect]
effects = [EffectA, EffectB, EffectC, EffectD, EffectE]

data Variable = VarV | VarW | VarX | VarY | VarZ
  deriving (Eq, Ord, Show)

instance Arbitrary Variable where
  arbitrary = elements variables

variables :: [Variable]
variables = [VarV, VarW, VarX, VarY, VarZ]
