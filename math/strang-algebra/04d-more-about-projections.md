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

So I am next interested in cases where there are `k<n` dimensions to
the subspace. In that case, the subspace is spanned by a basis of `k`
vectors. That means that `A` has `k` columns, but still has `n` rows,
because the subspace is embedded in `R^n`.

Likewise, we know that `A\trans` projects a vector onto each of `k`
basis vectors for the subspace. That means there are `k` rows, but `n`
columns, because this is a projection down from an `n`-dimensional
space to a `k`-dimensional space.

The important thing to note is that `AA\trans` is *square*. That's
because this maps vectors in `R^n` to vectors in the `k`-dimensional
subspace embedded in `R^n`, which still consists of `n`-tuples.

Again, `AA\trans` will not be invertible, because `A\trans` projects
down into a lower dimensionality space (doesn't have full row rank),
which immediately makes this not injective. Likewise, `A` is not
surjective, because it embeds a lower-dimensionality space into a
higher one. That's because `A` doesn't have full column-rank.
