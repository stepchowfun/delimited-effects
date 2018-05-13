# The Church encoding of a pair is a function which takes a binary function
# as an argument, applies it to the two elements, and returns the result.
pair = \x y f -> f x y
     : forall a b c .
       a -> b -> (a -> b -> c) -> c;
first = p -> p (\x y -> x)
     : forall a b .
       (forall c . (a -> b -> c) -> c) ->
       a;
second = p -> p (\x y -> y)
     : forall a b .
       (forall c . (a -> b -> c) -> c) ->
       b;

# A Boolean can be represented by a function which takes two arguments and
# returns one of them. We use 'yes' and 'no' here because 'true' and 'false'
# are native Booleans.
yes = \x y -> x
    : forall a . a -> a -> a;
no = \x y -> x
    : forall a . a -> a -> a;
not = \b x y -> b y x
    : forall a .
      (forall b . b -> b -> b) ->
      a -> a -> a;

# A natural number n can be represented by a function that maps a function to
# its nth iterate.
zero = \f x -> x
     : forall a .
       (a -> a) -> a -> a;
succ = \n f x -> f (n f x)
     : forall a .
       (forall b . (b -> b) -> b -> b) ->
       (a -> a) -> a -> a;
add = \m n f x -> m f (n f x)
     : forall a .
       (forall b . (b -> b) -> b -> b) ->
       (forall b . (b -> b) -> b -> b) ->
        (a -> a) -> a -> a;
mul = \m n f -> m (n f)
     : forall a .
       (forall b . (b -> b) -> b -> b) ->
       (forall b . (b -> b) -> b -> b) ->
        (a -> a) -> a -> a;
exp = \m n -> n m
     : forall a .
       (forall b . (b -> b) -> b -> b) ->
       (forall b . (b -> b) -> b -> b) ->
        (a -> a) -> a -> a;

# A list can be represented by its fold function (foldr in this example).
nil = \f i -> i
    : forall a b. (a -> b -> b) -> b -> b;
cons = \x l f i -> f x (l f i)
     : forall a b .
       a ->
       (forall c. (a -> c -> c) -> c -> c) ->
       (a -> b -> b) ->
       b ->
       b;

# This function converts a Church natural number into a native integer.
toInt = n -> n (x -> x + 1) 0
      : (forall a . (a -> a) -> a -> a) -> Int;

# This function converts a Church list into a native list.
toList = l -> l (\x y -> [x] ++ y) []
       : forall a . (forall b. (a -> b -> b) -> b -> b) -> [a];

# 3 ^ 4 = 81
three = succ (succ (succ zero)) : (forall a . (a -> a) -> a -> a);
four = succ three               : (forall a . (a -> a) -> a -> a);
eightyOne = exp three four      : (forall a . (a -> a) -> a -> a);

[toInt eightyOne] ++ toList (cons 82 (cons 83 nil))
