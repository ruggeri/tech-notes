**Linear Transformations/Matrices**

**Matrices** map linear combinations to linear combinations. Such a
map is called a **linear transformation**. The first column is what
the first basis vector maps to, the second basis vector maps to the
second column, etc.

The product of a matrix with a vector is the result of applying the
transformation to the vector. I like to see this as a weighted sum of
the columns.

We can *compose* transformations. This involves matrix
multiplication. It is quite simple. Given `BA`, take the first column
of `A`; that is what `e_1` maps to under `A`. Then apply `B` to this
column. This is now equal to what ought to be the first column of the
product `AB`. This suggestions a method of calculation.

Note to self: matrix multiplication involves `O(n**3)` time.

Another common way to apply a matrix to a vector is this: take the dot
product of each row in the matrix with the vector. What is the
intuition behind this? I suppose you could say this: the row is vector
which is "most" transformed to the `i`th basis vector. This is because
you can treat a single row in the matrix as a linear functional, and
the gradient defines the direction of steepest ascent.
