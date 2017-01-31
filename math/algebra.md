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

**Transposes**

So let's talk about transposes. I say that `A\trans x` maps `x` to its
projection on each of the columns of `A`. That's like almost literally
the definition.

We already showed that for any orthonormal matrix, the inverse is
equal to its transpose. For a matrix with skew, the transpose
`A\trans` does not properly invert `x`.

We say a matrix is **symmetric** if it is equal to its own transpose.

`RR\trans`

For **permutation matrices**, the transpose is also the inverse.

**TODO**: I feel like I really don't understand the transpose here.

## Ch3: Vector Spaces and Subspaces

Vector space is a space closed under linear combinations.

For a linear transformation, the *column space* defines all possible
values of `Ax`. It is of course a vector space. Naturally, we can only
solve `Ax=b` exactly when `b` lies in the column space. We can
calculate the `span` of the columns, which is exactly the smallest
vector space containing those columns.

A matrix also has a *null space*; these are vectors mapped to the zero
vector under the transformation. The zero vector is always in the null
space, but the null space can contain more vectors. Note that the null
space is itself subspace. If the null space has positive dimension,
then we say the matrix is *singular*.

Note: *dimension* is the size of the minimum size of a set of *basis
vectors* that spans a space. All minimal sets of basis vectors have
the same number of vectors, so dimension is well-defined. (Too lazy to
prove this).

A random matrix almost surely has *independent* columns; no column can
be written in terms of the others. This is exactly when the nullspace
has dimension zero.

**Calculation of the Null Space**
