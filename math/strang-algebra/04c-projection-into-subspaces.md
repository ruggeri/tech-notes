Let's talk projections into subspaces spanned by `{a_i}`. First, let's
note that `PP=P`. That's because, once you project into the subspace,
you stay there.

Some very simple projection matrices are `I`, and `I` but with some of
the diagonal zeroed-out (we drop that coordinate).

Okay. Say you want to project into a given subspace. Give me an
orthonormal basis for that subspace. Note that while multiply by a
matrix can be seen as a mapping from a linear combination of basis
vectors to a linear combination of matrix columns, you can also see it
as the projection of the vector onto each of the rows.

Therefore, we can write the basis vectors into matrix rows: let's call
this `A\trans`. Then we know that once we've done this projection, we
need to now map this vector of projection lengths to a linear
combination of those original vectors. Therefore, if we've written the
basis vectors as columns of `A`, we can do this by multiplying
`A\trans y` by `A`. Thus the matrix that does this entire thing is
`AA\trans`.

**One vector at a time**

Another way to approach this was to take our approach of projection
onto an individual basis vector, and then combine these
projections. That is, we could take the sum:

    \Sum_i (a_i a_i\trans)x

It's (at least a little) hard to see, but this is *exactly*
`AA\trans`. I will give a few reasons.

I've described matrix application in two ways. First, you can see an
input vector as describing how to do a linear combination of the
matrix columns. Another way is to see the rows as defining a linear
functional for one coordinate; you apply each row to calculate each
coordinate (this is the traditional approach).

Matrix multiplication is just a generalization of these mental
models. The typical way I think of `AB` is: `e_1` maps to the first
column of `B`; I then take the inner product with each of the rows of
`A` to determine what the first column of `B` maps to. However, I
could still think of this as a linear combination of the columns of
`A`. That would actually be more consistent with my normal view of
`Av`.

Let me propose a new way of seeing matrix multiplication `AB`. Apply
the first row of `B` to an input vector. This is the amount of the
first coordinate in `Bv`. Now, scale the first column of `A` by this
amount. Repeat and sum. This is exactly what I described above.
