module ErrorSpec
  ( errorSpec
  ) where

import Test.Hspec (Spec, describe, it, pending)

-- The QuickCheck specs
errorSpec :: Spec
errorSpec = describe "error" $ it "should be correct" $ pending
