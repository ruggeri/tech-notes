However, I want to be able to describe a projection *without* having
an orthonormal basis. For instance, say I have a `n`-by-`k` matrix,
with full row rank and column rank, but `k<n`. This matrix embeds
`R^k` into `R^n`. My goal is to project an arbitrary `y` onto the
image of the transformation.

First, let's assume that `y` is *in* the subspace. I would like simply
to find `x` such that `y = Ax`.

Now, we know that `x' := A\trans y` will not be correct if the columns
of `A` are not orthonormal. So what I would like to do is find `x`
from `x'`.

We don't know what `x` is yet, but we do know that `Ax=y`. Therefore,
`x' = A\trans y = A\trans(Ax)`. Therefore, we know that:

    x = (A\transA)\inv x' = (A\transA)\inv (A\trans y)

What is this mysterious `A\transA`? We know that this is a `k`-by-`k`
matrix, since it maps `R^k` to `R^n` and back again. Our next question
is: is it invertible? This is important because we know that
`AA\trans` is generally *not* inveritble.

**Invertibility of A\\transA**

Yes; `A\transA` is inveritble. First, note that `A` has zero
null-space. We assumed that `A` had full column-rank, which means that
the `k` columns are linearly independent. Having zero null-space means
that `A` is *injective*. Of course, `A` is *not surjective*, because
there aren't enough columns (`k`) to fill the entire target space
(`R^n`, for `n>k`).

Next, note that `A\trans` is just the opposite. We know that `A\trans`
is *not injective*, because it maps a higher-dimensionality space to a
lower dimensionality-space. Therefore, by definition, the columns of
`A\trans` cannot be linearly independent, because there are simply too
many of them.

On the other hand, `A\trans` is *surjective*. Remember how I said that
(1) because the columns of `A` were independent, (2) the nullspace of
`A` was zero? Well, remember that the nullspace is always the space
perpindicular to the rowspace. This shows that the rows of `A` span
`R^k`. Likewise, the columns of `A\trans` span `R^k`.

Another way to see the same thing. If the columns of `A` are
independent, that means the *rows* of `A\trans` are independent. That
means that for every row of `A\trans` there is a vector perpindicular
to every other row besides this one. (The proof of that statement
could come from orthogonaliztion of the basis). A vector `v_i` in
`R^n` that is not perpindicular to row `i` but is perpindicular to all
other rows of `A\trans` maps to a multiple of `e_i` in `R^k`; so these
`v_i` form a basis of `R^k`.

(In general, the nullspace of `M` is orthogonal to the columnspace of
`M\trans`.)

Next I'll show that `A\trans` is injective on `Im(A)`. I'll prove this
more generally: any matrix `M` is injective on the rowspace of `M`. If
two vectors map to the same value, then their difference is in the
nullspace. As we know, the nullspace is perpindicular to the rowspace
of `M`. Therefore, there is only one vector in the rowspace that hits
a value in the image of the transformation: the one with exactly zero
component when projected into the nullspace.

(Note that showing that `A\trans` is injective on `Im(A)` would be
enough to establish surjectivity in this case, because the image of
`A` has rank `k`, and thus the injectivity of `A\trans` implies its
image still has rank `k`, which is the rank of the entire target
space).

This was an exhaustive proof of a relatively simple fact: `A\transA`
is invertible.

**If the columns of A are orthogonal**

In that case, no unskewing is needed. Then `A\transA=I`.

**Projecting Points Outside the Subspace**

Okay. I gave you a proof that if `y=Ax`, then:

    x = (A\trans A)\inv (A\trans y)

But that's sort-of useless. What I wanted to do was project points
*off the subspace*. But note that projection onto the subspace means
given `y`, finding a `y'` such that `y = y' + q`, where `q` is a
vector perpindicular to the subspace.

In that case, we need only note that:

    A\trans y = A\trans (y' + q) = A\trans y' + 0 = A\trans y'

The reason is that the `q` part is perpindicular to the columnspace of
`A`, and therefore is in the nullspace of `A\trans`.

This once-and-for-all establishes that to perform the projection, you
can find the corresponding `x` by:

    x = (A\trans A)\inv (A\trans y)

And from here you can find the corresponding `y'` via `Ax`, meaning:

    y' = A (A\trans A)\inv (A\trans y)

**Pseudo-Inverse**

Basically, the matrix `(A\trans A)\inv A\trans` is a *pseudo-inverse*
for `A`. I say pseudo-inverse because since `A` is not surjective, it
doesn't have a true inverse. But `A` is injective on its image, so
this matrix is an inverse on this image.

You can think of the action `A` on `v` as: decompose `v` into rows of
`A`; this is an alternative way to view any linear
transformation. Subsequently applying `A\trans` to `Av` attempts to
reassemble `v` from the decomposition.

But the reassembly is not quite right. This is what `(A\trans A)\inv`
is fixing.

Brain storm: for any matrix `A` where you want to invert, `A\inv =
(A\trans A)\inv A\trans`; for orthogonal matrices the `A\trans A` part
is already the identity. But for other matrices it is vital and does
the unmixing.

Note! All the hardness of doing inversion is exactly in inverting a
matrix `A\trans A`. That means: the difficulty of inversion is in
performing *unskewing*.

**Mixing Matrix**

The matrix `A\trans A` is called a *Gramian matrix* (as in
Gram-Schmidt). It maps `e_i` to their image under decomposition and
reconstruction by `A`.  The reconstruction depends on the extent to
which the rows of `A` are not orthonormal. It makes sense that `e_i`
maps to `(a_1 \cdot a_i, a_2 \cdot a_i, ..., a_n \cdot a_i)`.

The matrix `(A\trans A)\inv` undoes this. It takes that vector `(a_1
\cdot a_i, ..., a_n \cdot _ai)` and maps it back to `e_i`.

