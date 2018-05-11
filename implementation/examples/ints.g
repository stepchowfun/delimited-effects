# Arithmetic respects the familiar order of operations.
twentyThree = 3 + 4 * 5;

# Division rounds down. Division by zero is a runtime error.
eleven = twentyThree / 2;

# This function returns the square of the hypotenuse of a right triangle.
squaredHypotenuse = \x y -> x * x + y * y;

# Compute the square of the hypotenuse of the 3 : 4 : 5 triangle.
squaredHypotenuse 3 4
