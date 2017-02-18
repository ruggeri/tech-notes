Let's talk projections into subspaces spanned by `{a_i}`. First, let's
note that `PP=P`. That's because, once you project into the subspace,
you stay there.

Some very simple projection matrices are `I`, and `I` but with some of
the diagonal zeroed-out (we drop that coordinate).

Okay. Say you want to project into a subspace. Give me an orthonormal
basis for that subspace. Write the orthonormal basis as columns of a
matrix `A`. Then `AA\trans` is exactly the projection into the
subspace.

Note that `A` is square if and only if you've taken the *entire space*
as your subspace. The projection is just the identity in this case. So
`AA\trans=I`. In fact, this means that `A` is an *orthogonal matrix*,
and we previously discussed how this is just a rotation (plus flips),
and its transpose is its inverse...

So the interesting case is if your subspace isn't the entire space. In
that case, `A` is a rectangular matrix. However, `AA\trans` is of
course a square matrix. It doesn't have full rank, though. Its rank is
equal to the size of the subspace.

**Could have done this one basis vector at a time**

Let's note this about `AA\trans`. We could have done the projection of
`x` onto each of the `a_i` one at a time. That is, we could take the
sum:

    \Sum_i (a_i a_i\trans)x

In fact, this is another way to see matrix multiplication. Normally I
think of `BA` as: first see what `e_i` would map to under `A` (first
column). Then I take the dot-product of this `Ae_i` with each of the
rows of `B`. That's funny: even though I try to think of `Av` as a
linear combination of the columns of `A`, I *don't* think like that
for `BA`, though I could. But regardless, my normal way is: follow
`e_i` through `BA` and write this as the `i`th column.

What we're seeing here is another way to view matrix multiplication,
which is project `x` onto the first row, this is how much you have in
the first coordinate under `A`, then multiply this by the first column
of `B`. This operation is summarized by the outer product of the first
column and row.

**Symmetry**

Now, `uu\trans` can very easily be verified as symmetric. Since
`AA\trans` is just the sum of symmetric matrices, then we know this is
symmetric as well.

**TODO**: Clean up these thoughts. I'm on page 210 in Strang.

We can also show this like so: `P\trans=(AA\trans)\trans=AA\trans`.

But that's a stupid way to show this, perhaps. There must be a better
intuition for this.

My question now is: are all symmetric matrices doing projection? I
don't think so... But maybe if they have det=1?

But this takes us to the next important thing. Since `A\trans`
computes the lengths of the projections onto the column vectors, then
`AA\trans` is the projection of the vector into the column space.

But this only holds if the column vectors are orthogonal (it's okay
for some to be zero).

This concept of projection calculates the point in the subspace that
is closest to the original point. That is because the error is
entirely perpindicular to the columnspace, and moving in the
columnspace won't reduce this part of the error, it'll just introduce
new error in the subspace.
