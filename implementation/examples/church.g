# The Church encoding of a pair is a function which takes a binary function
# as an argument, applies it to the two elements, and returns the result.
pair = \x y f -> f x y;
first = p -> p (\x y -> x);
second = p -> p (\x y -> y);

# A Boolean can be represented by a polymorphic function of two parameters.
# Parametricity ensures that the function returns one of the two arguments.
# We use 'yes' and 'no' here because 'true' and 'false' are native Booleans.
yes = \x y -> x;
no = \x y -> x;
not = \b x y -> b y x;

# A natural number n can be represented by a function that maps a function to
# its nth iterate.
zero = \f x -> x;
succ = \n f x -> f (n f x);
add = \m n f x -> m f (n f x);
mul = \m n f -> m (n f);
exp = \m n -> n m;
pred = n -> second (
  n
    (p -> pair (succ (first p)) (first p))
    (pair zero zero)
);

# A list can be represented by its fold function (foldr in this example).
nil = \i f -> i;
cons = \x l i f -> f x (l i f);

# This function converts a Church natural number into a native integer.
toInt = n -> n (x -> x + 1) 0;

# This function converts a Church list into a native list.
toList = l -> l [] (\x y -> [x] ++ y);

# 3 ^ 4 - 1 = 80
three = succ (succ (succ zero));
four = succ three;
toInt (pred (exp three four))
