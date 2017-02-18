We talked about how to project onto an orthonormal basis `a_i`. This
was just `AA\trans`.

It was important that every pair of vectors are orthogonal. Otherwise,
we know the decomposition into the basis vectors won't quite work,
because of double counting.

So, let's say you have a subspace of `R^n`. If you have an orthonormal
basis of `n` vectors, this must span the entire space. So the
projection into this space is the identity. Therefore,
`AA\trans=I`. This harkens back to our discussion of orthogonal
matrices, where we already saw this very same property that
`A\inv=A\trans`.

So I am next interested in cases where there are fewer than `n`
dimensions to the subspace. In that case, we know the matrix `A`
has column rank strictly less than `n`.

So, if we write in the `a_i` as rows of `A\trans`, this is a
rectangular matrix, where the number of rows is less than the number
of columns. Likewise, `A` is just the opposite; the number of columns
is less than the number of rows.

Of course, `AA\trans` is square. This can of course be verified by
computation, but the idea is that we're projecting members of `R^n`
into a subspace of lower dimensionality, but it still may use all the
coordinates. It's just that the coordinates aren't linearly
independent.

Again, `AA\trans` will not be invertible, because `A\trans` projects
down into a lower dimensionality space (doesn't have full row rank),
which immediately makes this not injective. Likewise, `A` is not
surjective, because it embeds a lower-dimensionality space into a
higher one. That's because `A` doesn't have full column-rank.
