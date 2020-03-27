**Elementary Matrices**

All *invertible* linear transformations can be decomposed into a
product of *elementary matrices*. These are:

1. Matrix that swaps two coordinates.
2. Matrix that scales a coordinate.
3. Matrix that adds a coordinate to another. This is a *shear
   mapping*.

The first kind changes the sign of the determinant, the second changes
the magnitude of the determinant, and the third has no impact.

If we add in a fourth kind of elementary matrix, which zeros out a
coordinate, then I do believe this generates all matrices.

**Row-Operations vs Column-Operations**

When we perform a row operation, we undo this by multiply by an
elemtary matrix on the left side. This is a form of decomposition of
the matrix.

If we are doing Gaussian elimination, our operations are always to add
a scalar multiple of a row `i` to another row `j>i`. The row operation
matrix that performs this has a single off-diagonal entry, which is at
position `(j, i)`. What this says is: add this much of the `i`th
coordinate back to the `j`th coordinate, which will make up for our
removal.

Since `j>i`, this is a lower triangular matrix.

We could also do the same thing but by doing column operations. If we
subtracted a column `i` from a later column `j`, we need a matrix
which has an entry *above* the diagonal, and we need to apply this
column operation *before* the reduced matrix.
