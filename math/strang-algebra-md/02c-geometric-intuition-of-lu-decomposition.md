Geometrically, what we are doing is this. We are saying: I want `e_1`
to only impact the first coordinate of `b`; I want to eliminate its
effect on the other coordinates. Then we say: good, now I want e_2 to
only involve itself with the second coordinate in the image space.

What is the idea of such a transformation? Well, it is this: to write
a matrix as a pair of matrices, the first of which sends

    e_i -> Sum_{j<=i} e_j

and the second which sends

    e_i -> Sum_{j>=} e_j
