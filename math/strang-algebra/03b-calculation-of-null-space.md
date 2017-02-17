I think the way to do this is to start to perform elimination to form
the `LU` decomposition. Notice that as we change `A` into `U`, all
these changes are being "undone" by `L`. So the nullspace of `A` is
exactly the same as the nullspace of `U`.

We know that sometimes we have to do pivots to swap rows when we're
doing eliminiation. But sometimes there is no pivoting we can do. When
this happens, it shows that this column lies in the subspace of the
previous columns.

In that case, we work our way backward subtracting out just enough of
one column to eliminate the next coordinate of this column. This shows
how to recover one of the basis vectors of the null space.

Note that there is no need to keep track of `U` through this process.

We call `U` the *echelon matrix*. We can do some further
decomposition. We can subtract later rows from earlier ones to "clear
out" entries above *pivots* (a pivot is the first non-zero element in
the row).

This is kind of like factoring `U` into a `U'L'` decomposition. Again,
we are focused on the `L'` matrix. Further, we can make all pivots
`1`, which is equivalent to factoring out a scaling by a diagonal
matrix. This is called *row reduced echelon form*.

The number of pivots is called the *rank* of the matrix, which is of
course the size of the column-space.

It should be clear that the dimension of the column-space and the
dimension of the nullspace sum to the dimension of the input
space. That's because if we consider each basis vector one-by-one, it
either (1) doesn't map into the previous columns' column-space, in
which case the dimensionality of that space has increased, or (2) it
does. If it does, then there is some linear combination of the prior
basis vectors with this one that maps to zero. This is a new member of
the null space because no vector of the previous null space ever had a
one in the coordinate of this basis vector.

It's kind of a confusing explanation, but I do feel confident in this
idea.

It should also be clear that any member of the null space is
perpindicular to the row space. That's because projections onto the
row space vectors give the magnitude of a coordinate under the
transformation. So these must all be zero if `v` does lie in the
nullspace.
