The determinant is the volume of the the unit box's image under the
transformation. That is: if you consider the columns, one by one,
collecting the product of the norm of the column, minus its projection
into the space spanned by the prior columns, this is the determinant.

**TODO**: When should we flip the signs?

# Obvious Properties

* Determinant of identity is one.
* Scaling a column scales the determinant (scales the component of
  this column orthogonal to all other columns).
* Adding a column i to another column j doesn't change determinant,
  because column i won't contribute to the orthogonal
  component of column j.
* If two columns are equal, determinant must be zero because they have
  no orthogonal component to the other rows.

*Triangular Matrix*
Determinant of a triangular matrix is simple; it's the product of the
diagonal. That's easy to see, start with the row with one entry, and
then work your way through the other rows; all but the entry on the
diagonal is parallel to the dimension already defined.

*Singular Matrix*
A matrix is singular (that is, not injective) iff the determinant is
zero. The determinant is zero if some row lies in the span of the
other rows, which is the definition of not invertible.

*Calculation by Pivots*
This recommends a practical way of calculating the determinant. We do
elimination, which doesn't change the determinant (except pivoting),
and then multiply the pivots.

Actually, everything I said about rows is also true of columns. In
fact, it makes more sense to me as colunns.

## Big Formula

Here's the idea. The determinant is *linear* in the sense that you can
break up a column. For instance, say `A` has column vectors `a_1, a_2,
a_3`. Further, say that `a_1 = a_11 + a_12 + a_13`. Then:

    det([a_1, a_2, a_3]) = det([a_11, a_2, a_3]) + det([a_12, a_2, a_3]) + det([a_13, a_2, a_3])

Here, I'm trying to indicate replacing `a_11` with a vector that has
`a_1i` in the ith position and otherwise zeros.

The linearity of the determinant is obvious. The amount of `a_1`
orthogonal to `a_2, a_3` is merely distributed to the three
sub-calculations.

Further, we can pull out the `a_1i`:

    a_11det([1, a_2, a_3]) + a_12det([1, a_2, a_3]) + a_13det([1, a_2, a_3])

We can continue this, in which case the sum looks like:

    \Sum a_1i * a_2j * a3k * det(matrix with ones @ 1i, 2j, 3k)

The sum needs to happen with `i \ne j \ne k`; that is, we have a term
per sequence in `S_n` (n is the dimension of the matrix).

Note that the determinant we have left is just a pivot matrix, so this
has determinant `+/-1`.

## Cofactor Formula

This is sort of a simplification of the big formula. Let's return to this:

    a_11det([1, a_2, a_3]) + a_12det([1, a_2, a_3]) + a_13det([1, a_2, a_3])

Note that in the first term, we can ignore `a_21, a31`; these will add
nothing to the volume. In that case, we might as well just find the
volume of the 2-D submatrix left.

This is kinda like saying "I've pulled out the height, and I'll
multiply this by the area of base times width." One trick to remember
is that since we're talking about signed area, we have to keep in mind
any swapping we do. So, the cofactor is the determinant of the
eliminated matrix, times `-1^(1+i)`, where `1+i` is the number of row
swaps needed to bring `a1i` up to the top row but leave everything
else in order.

## Cramer's Rule

Cramer's rule says that to solve `Ax=b`, you introduce matrices `B_i`
which replace the ith column of `A` with `b`. Then you set

    x_i = det(B_i)/det(A)

I think the idea is this. This is basically saying: `a_i` contributes
to `det(A)` insomuch as it is orthogonal to the other rows. Now, if we
replace it with `b`, the part of `b` that lies in the space of the
other rows stays ignored. But there is a part of `b` orthogonal to the
other rows; this can only be accounted for by using `a_i`. How much of
`a_i`? Well, if replacing `a_i` by `b` scaled the determinant by yea
much, that means that `b` is that many times longer than `a_i` in the
orthogonal direction. Thus we must use `b` this many times.

The book makes clear that this isn't a practical way to solve a system
of equations.

**TODO**: He defined the cross product in terms of cofactors. I
haven't had a reason to care about this particularly.
