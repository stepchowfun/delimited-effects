# Here's a function which concatenates some lists.
f = \x y -> x ++ [3] ++ y;

# We can apply it to a couple of arguments.
list = f [1, 2] [4, 5];

# This function takes anything and puts it into a list.
singleton = x -> [x];

# We can specialize the singleton function to work only on values that have the
# same type as the polymorphic identity function. This essentially means it
# only works on identity functions due to parametricity.
idSingleton = singleton : (forall a . a -> a) -> [forall a . a -> a];

# Now we can put the polymorphic identity function into a list.
idSingleton (x -> x)
