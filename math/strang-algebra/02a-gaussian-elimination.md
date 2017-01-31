## Ch2: Solving Linear Equations

A system of linear equations can be seen as a matrix. Each equation is
a coordinate in the new space. Each equation defines a row in the
matrix. It's how much each coordinate in the original space
contributes to the value of the second space.

If we view the problem like this, then the solution is to take the
target in the transformed space and find its inverse under the
transformation.

The typical way to find the solution is to perform **Gaussian
Elimination**. What this does is slowly multiply each side of `Ax=b`
by a set of basic transformations until `A` is eliminated and `b` has
been transformed to its inverse under `A`.

The operations are these:

1. Scale a row.
2. Add or subtract a multiple of a row from another.
3. Swap rows.

The algorithm is like this:

1. Start with the first row. Scale so its initial entry is `1`.
    * (For each transformation to `A`, perform the same transformation
      to `b`).
2. Subtract the appropriate amount from the other rows so that you
   eliminate the other entries in the first column.
3. Move to the second row. Repeat the process: scale so that the
   second element in the second row is 1, and then subtract as needed
   from the other columns.
4. The only problem is if you encounter the `i`th row and it has `0`
   at the `i`th column. In that case, swap two rows.

If you apply the transformations to `b`, you will end up with convert
it to its inverse. You may instead apply these transformations to `I`,
which will convert this into the inverse linear transformation.

Note: the version where we transform `I` can be seen as finding the
inverse for *n* vectors (the columns) simultaneously. That's kind of
interesting: to see it as nothing more than a parallelized/vectorized
version of solving for one inverse.

You may end up in a situation where you have only zeros left in a
column, so that no pivot can help you. In that case, this is a
*singular* matrix, and it doesn't have a proper inverse. We'll talk
about that later.
