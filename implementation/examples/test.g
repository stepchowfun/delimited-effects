# This is the Church encoding of lists.
nil = \i f -> i
  : forall a b
  . b
  -> (a -> b -> b)
  -> b in

cons = \x l i f -> l (f x i) f
  : forall a
  . a
  -> (forall b . b -> (a -> b -> b) -> b)
  -> forall b . b -> (a -> b -> b) -> b in

# This computes the sum of a list of integers.
sum = l -> l 0 (\x y -> x + y)
  : (forall a . a -> (Int -> a -> a) -> a)
  -> Int in

# Here's a list containing three numbers.
myList = cons 3 (cons 4 (cons 5 nil)) in

# Let's take its sum.
myList
