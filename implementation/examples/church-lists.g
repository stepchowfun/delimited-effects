# This Church encoding of lists is just folds (foldr in this example).
nil = \i f -> i
  : forall a b . b -> (a -> b -> b) -> b;

cons = \x l i f -> f x (l i f)
  : forall a b
  . a
  -> (forall c . c -> (a -> c -> c) -> c)
  -> b -> (a -> b -> b) -> b;

# This computes the sum of a list of integers.
sum = l -> l 0 (\x y -> x + y)
  : (forall a . a -> (Int -> a -> a) -> a) -> Int;

# Here's a list containing three numbers.
myList = cons 3 (cons 4 (cons 5 nil));

# Let's take its sum.
total = sum myList;

# This function converts a Church list into a native list.
toNative = l -> l [] (\x y -> [x] ++ y)
  : forall a . (forall b . b -> (a -> b -> b) -> b) -> [a];

# Convert the Church list to a native list and append the sum.
toNative myList ++ [total]
