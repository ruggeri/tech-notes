**Inverting A Matrix**

How do we "undo" a linear transformation? That is, given a vector `v`
in terms of `B`, how do we find the vector `u` such that `v=Au`?

This is easy if we decompose `v` into a linear combination of the
columns of `A`. If the columns of `A` are orthogonal in the geometry
implied by `B`, we can do this by using the dot product. If, as is
typical, `B` is not orthogonal, then we must do more work.

So let's take a moment to consider a matrix that maps the basis to
another orthonormal basis. To invert an element `v`, we need to
decompose it into columns of `A`. But since the columns are
orthonormal, we can use the dot product just as we said. So `e_1`
should map to a vector which is `(<e_1, A_1>, <e_1, A_2>, ...)`; that
is, we project `e_1` onto each column of `A`. Of course, this is just
the first row of `A`.

By definition of how a linear transformation is represented, we write
this vector of projects as the first column. Thus, the first row has
become the first column.

This demonstrates that any orthogonal matrix (which represents
rotations + flips) always has its inverse equal to its transpose! Thus
we write:

    Q\inv=Q\trans

Since, `Q` is often used for an orthogonal matrix.
