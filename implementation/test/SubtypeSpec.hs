module SubtypeSpec (subtypeSpec) where

import Lib (Row(..), Type(..), subrow, subtype)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (property)
import Types (Effect(..))

-- The QuickCheck specs

specMonotonicity :: Type Effect
                 -> Row Effect
                 -> Type Effect
                 -> Row Effect
                 -> Bool
specMonotonicity t1 r1 t2 r2 =
     not (subtype t1 REmpty t2 REmpty || t1 == t2)
  || not (subrow r1 r2)
  || subtype t1 r1 t2 r2

subtypeSpec :: Spec
subtypeSpec = modifyMaxSuccess (const 100000) $ describe "subtype" $ do
  it "is monotonic with respect to nested subtypes or subrows" $ do
    property specMonotonicity
