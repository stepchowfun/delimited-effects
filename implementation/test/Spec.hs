import Data.List (foldl')
import Data.Stream (Stream(..), head, tail)
import Lib (Row(..), equivalent)
import Test.Hspec (describe, hspec, it, pending)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (
    Arbitrary,
    arbitrary,
    elements,
    property,
    shrink,
    suchThat
  )

data Effect = EffectA | EffectB | EffectC | EffectD | EffectE
  deriving (Eq, Ord, Show)

instance Arbitrary Effect where
  arbitrary = elements effects

effects = [EffectA, EffectB, EffectC, EffectD, EffectE]

data Variable = VariableV | VariableW | VariableX | VariableY | VariableZ
  deriving (Eq, Ord, Show)

instance Arbitrary Variable where
  arbitrary = elements variables

variables = [VariableV, VariableW, VariableX, VariableY, VariableZ]

data Context = Context
  { getV :: Row Variable Effect
  , getW :: Row Variable Effect
  , getX :: Row Variable Effect
  , getY :: Row Variable Effect
  , getZ :: Row Variable Effect
  } deriving (Eq, Show)

-- This type represents a context whose rows are closed.
newtype ClosedContext = ClosedContext Context
  deriving Show

instance Arbitrary ClosedContext where
  arbitrary = ClosedContext <$> (
      Context <$>
        (suchThat arbitrary closed) <*>
        (suchThat arbitrary closed) <*>
        (suchThat arbitrary closed) <*>
        (suchThat arbitrary closed) <*>
        (suchThat arbitrary closed)
    )
  shrink (ClosedContext c) = [
      ClosedContext (Context v' w' x' y' z') |
      (v', w', x', y', z') <- shrink (getV c, getW c, getX c, getY c, getZ c)
    ]

closed :: Row a b -> Bool
closed (RVariable x) = False
closed REmpty = True
closed (RSingleton x) = True
closed (RUnion x y) = closed x && closed y
closed (RDifference x y) = closed x && closed y

substitute :: Context -> Row Variable Effect -> Row Variable Effect
substitute c (RVariable x) = case x of
  VariableV -> getV c
  VariableW -> getW c
  VariableX -> getX c
  VariableY -> getY c
  VariableZ -> getZ c
substitute c REmpty = REmpty
substitute c (RSingleton x) = RSingleton x
substitute c (RUnion x y) = RUnion (substitute c x) (substitute c y)
substitute c (RDifference x y) = RDifference (substitute c x) (substitute c y)

-- The row is assumed to be closed.
contains :: Effect -> Row Variable Effect -> Bool
contains e (RVariable x) = error "The row is not closed."
contains e REmpty = False
contains e (RSingleton x) = x == e
contains e (RUnion x y) = contains e x || contains e y
contains e (RDifference x y) = contains e x && not (contains e y)

occurs :: Effect -> Row Variable Effect -> Bool
occurs e (RVariable x) = False
occurs e REmpty = False
occurs e (RSingleton x) = x == e
occurs e (RUnion x y) = occurs e x || occurs e y
occurs e (RDifference x y) = occurs e x || occurs e y

allOccur :: Row Variable Effect -> Bool
allOccur x = all (\e -> occurs e x) effects

specForward :: ClosedContext ->
               Row Variable Effect ->
               Row Variable Effect ->
               Bool
specForward (ClosedContext c) x y =
  let
    r1 = substitute c x
    r2 = substitute c y
  in
    all (\e -> contains e r1 == contains e r2) effects ||
      not (equivalent x y)

specReverse :: Row Variable Effect -> Row Variable Effect -> Bool
specReverse p q = allOccur p || allOccur q || not (all id (
    do
      v <- effects
      w <- effects
      x <- effects
      y <- effects
      z <- effects
      let v' = RSingleton v
      let w' = RSingleton w
      let x' = RSingleton x
      let y' = RSingleton y
      let z' = RSingleton z
      let c = Context v' w' x' y' z'
      let r1 = substitute c p
      let r2 = substitute c q
      return (all (\e -> contains e r1 == contains e r2) effects)
  )) || equivalent p q

main :: IO ()
main = hspec $ do
  describe "equivalent" $ do
    modifyMaxSuccess (const 1000) $
      it "returns False for any two rows if they contain different effects \
        \after some closed substitution" $ property specForward
    modifyMaxSuccess (const 100000) $
      it "returns True for any two rows if they contain the same effects \
        \after all possible singleton substitutions" $ property specReverse
