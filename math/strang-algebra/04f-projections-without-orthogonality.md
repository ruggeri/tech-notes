However, I want to be able to describe a projection *without* having
an orthonormal basis. For instance, say I have a `k`-by-`n` matrix,
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

Yes. First, note that `A` has zero null-space. We assumed that `A` had
full column-rank, which means that the `k` columns are linearly
independent. Having zero null-space means that `A` is *injective*. Of
course, `A` is *not surjective*, because there aren't enough columns
(`k`) to fill the entire target space (`R^n`, for `n>k`).

Next, note that `A\trans` is just the opposite. We know that `A\trans`
is *not injective*, because it maps a higher-dimensionality space to a
lower dimensionality-space. Therefore, by definition, the columns of
`A\trans` cannot be linearly independent, because there are simply too
many of them.

On the other hand, `A\trans` is *surjective*. Remember how I said that
because the columns of `A` were independent the nullspace of `A` was
zero? Well, remember that the nullspace is always the space
perpindicular to the rowspace. This shows that the rows of `A` span
`R^k`. Likewise, the columns of `A\trans` span `R^k`.

(In general, the nullspace of `M` is orthogonal to the columnspace of
`M\trans`.)

Next I'll show that `A\trans` is injective on `Im(A)`. I'll prove this
more generally: any matrix `M` is injective on the rowspace of `M`. If
two vectors map to the same value, then their difference is in the
nullspace. As we know, the nullspace is perpindicular to the rowspace
of `M`. Therefore, there is only one vector in the rowspace that hits
a value in the image of the transformation: the one with exactly zero
component when projected into the nullspace.

(Note that showing that `A\trans` is injective would be enough to
establish surjectivity in this case, because the image of `A` has rank
`k`, and thus the injectivity of `A\trans` implies its image still has
rank `k`, which is the rank of the entire target space).

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

**Meaning of A\\transA**

In case it is not obvious, the matrix `A\transA` takes an `x` and maps
it to a new `x` that is the skewed projection of `y` onto the column
basis.  Thus the matrix `(A\transA)\inv` is the matrix that does the
opposite skewing to undo this.

**TODO**: I am still stuck around page 210.
