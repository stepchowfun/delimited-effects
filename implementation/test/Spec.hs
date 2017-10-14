import Data.List (foldl')
import Lib (Row(..), rowEquiv)
import Test.Hspec (describe, hspec, it, pending)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (Arbitrary, arbitrary, elements, property)

data Effect = EffectA | EffectB | EffectC | EffectD | EffectE
  deriving (Eq, Show)

effects = [EffectA, EffectB, EffectC, EffectD, EffectE]

instance Arbitrary Effect where
  arbitrary = elements effects

data Variable = VariableV | VariableW | VariableX | VariableY | VariableZ
  deriving (Eq, Show)

variables = [VariableV, VariableW, VariableX, VariableY, VariableZ]

instance Arbitrary Variable where
  arbitrary = elements variables

data Context = Context {
    getV :: Row Effect Variable,
    getW :: Row Effect Variable,
    getX :: Row Effect Variable,
    getY :: Row Effect Variable,
    getZ :: Row Effect Variable
  } deriving (Eq, Show)

instance Arbitrary Context where
  arbitrary = do
    v <- arbitrary
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Context v w x y z

closed :: Row a b -> Bool
closed (RVar x) = False
closed REmpty = True
closed (RSingleton x) = True
closed (RUnion x y) = closed x && closed y
closed (RDifference x y) = closed x && closed y

substitute :: Row Effect Variable -> Context -> Row Effect Variable
substitute (RVar x) c = case x of
  VariableV -> getV c
  VariableW -> getW c
  VariableX -> getX c
  VariableY -> getY c
  VariableZ -> getZ c
substitute REmpty c = REmpty
substitute (RSingleton x) c = RSingleton x
substitute (RUnion x y) c = RUnion (substitute x c) (substitute y c)
substitute (RDifference x y) c = RDifference (substitute x c) (substitute y c)

-- The row is assumed to be closed.
contains :: Effect -> Row Effect Variable -> Bool
contains e (RVar x) = error "The row is not closed."
contains e REmpty = False
contains e (RSingleton x) = x == e
contains e (RUnion x y) = contains e x || contains e y
contains e (RDifference x y) = contains e x && not (contains e y)

spec :: [Context] -> Row Effect Variable -> Row Effect Variable -> Bool
spec cs x y =
  let r1 = foldl' substitute x cs
      r2 = foldl' substitute y cs
  in not (closed r1) || not (closed r2) ||
     rowEquiv x y == all (\e -> contains e r1 == contains e r2) effects

main :: IO ()
main = hspec $ modifyMaxSuccess (const 1000000) $ do
  describe "rowEquiv" $ do
    it "returns True iff the rows contain the same set of effects for any \
      \substitution" $ pending -- property spec
