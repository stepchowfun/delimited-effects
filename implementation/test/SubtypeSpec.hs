module SubtypeSpec (subtypeSpec) where

import Lib (Type(..), subtype)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (property)
import Types (Effect(..))

-- The QuickCheck specs

specReflexivity :: Type Effect -> Type Effect -> Bool
specReflexivity t1 t2 = not (t1 == t2) || subtype t1 t2

specTransitivity :: Type Effect -> Type Effect -> Type Effect -> Bool
specTransitivity t1 t2 t3 =
     not (subtype t1 t2)
  || not (subtype t2 t3)
  || subtype t1 t3

subtypeSpec :: Spec
subtypeSpec = modifyMaxSuccess (const 100000) $ describe "subtype" $ do
  it "is a reflexive relation" $ do
    property specReflexivity
  it "is a transitive relation" $ do
    property specTransitivity
