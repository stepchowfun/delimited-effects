module Error
  ( Partial
  , abort
  , assert
  , maybeToPartial ) where

type Partial a = Either String a

abort :: String -> Partial a
abort s = Left s

assert :: Bool -> String -> Partial ()
assert b s = if b then return () else abort s

maybeToPartial :: Maybe a -> String -> Partial a
maybeToPartial (Just x) _ = return x
maybeToPartial Nothing  s = abort s
