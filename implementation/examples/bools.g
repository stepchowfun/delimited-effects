# We have conditionals.
three = if true then 3 else 4;

# Type inference gives this type for the following function:
#   ∀α . Bool → α → α → α
\x y z -> if x then y else z
