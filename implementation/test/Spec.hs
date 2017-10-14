import Data.List (foldl')
import Data.Stream (Stream(..), head, tail)
import Lib (Row(..), rowEquiv)
import Test.Hspec (describe, hspec, it, pending)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (Arbitrary, arbitrary, elements, property, shrink)

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

data Context = Context
  { getV :: Row Effect Variable
  , getW :: Row Effect Variable
  , getX :: Row Effect Variable
  , getY :: Row Effect Variable
  , getZ :: Row Effect Variable
  } deriving (Eq, Show)

instance Arbitrary Context where
  arbitrary = Context <$>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary
  shrink c = [
      Context v' w' x' y' z' |
      (v', w', x', y', z') <- shrink (getV c, getW c, getX c, getY c, getZ c)
    ]

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

-- QuickCheck has a bug where it tries to print the arguments for a failed
-- test case, even if they are infinite (which causes the program to hang).
-- We use infinite Streams, so we need a newtype wrapper to work around this
-- QuickCheck bug. We define a special Show instance which returns only a
-- prefix of the (infinite) representation of the Stream.
newtype ContextStream = ContextStream (Stream Context)

streamShrink :: Arbitrary s => Int -> Stream s -> [Stream s]
streamShrink n s = if n == 0 then [] else do
  t <- [s] ++ streamShrink (n - 1) (Data.Stream.tail s)
  h <- shrink (Data.Stream.head s)
  return (Cons h t)

instance Arbitrary ContextStream where
  arbitrary = ContextStream <$> (Cons <$> arbitrary <*> arbitrary)
  shrink (ContextStream s) = ContextStream <$> streamShrink 8 s

instance Show ContextStream where
  show (ContextStream s) = take 1024 (show s) ++ "..."

csHead :: ContextStream -> Context
csHead (ContextStream s) = Data.Stream.head s

csTail :: ContextStream -> ContextStream
csTail (ContextStream s) = ContextStream (Data.Stream.tail s)

subUntilClosed :: ContextStream -> Row Effect Variable -> Row Effect Variable
subUntilClosed cs x = fst $ until
  (closed . fst)
  (\(r, s) -> (substitute r (csHead s), csTail s))
  (x, cs)

spec :: ContextStream -> Row Effect Variable -> Row Effect Variable -> Bool
spec cs x y =
  let
    r1 = subUntilClosed cs x
    r2 = subUntilClosed cs y
  in
    rowEquiv r1 r2 == all (\e -> contains e r1 == contains e r2) effects

main :: IO ()
main = hspec $ modifyMaxSuccess (const 1000000) $ do
  describe "rowEquiv" $ do
    it "returns True iff the rows contain the same set of effects for any \
      \substitution" $ pending -- property spec
