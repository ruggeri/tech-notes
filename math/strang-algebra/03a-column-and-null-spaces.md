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
