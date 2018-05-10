# This is the Church encoding of lists.
nil = \i f -> i
  : forall a b
  . b
  -> (a -> b -> b)
  -> b;

cons = \x l i f -> l (f x i) f
  : forall a b
  . a
  -> (forall c . c -> (a -> c -> c) -> c)
  -> b -> (a -> b -> b) -> b;

# This computes the sum of a list of integers.
sum = l -> l 0 (\x y -> x + y)
  : (forall a . a -> (Int -> a -> a) -> a)
  -> Int;

# Here's a list containing three numbers.
myList = cons 3 (cons 4 (cons 5 nil));

# Let's take its sum.
sum myList
