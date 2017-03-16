We showed using algebra that the best `theta` is found by:

    (X\trans X)\inv X\trans y

I want to explore the meaning of this equation more deeply.

**Decomposing x into X**

We can see the problem of prediction as a problem of
*interpolation*. We want to describe a point `x` in terms of
previously observed `x_i` and then do the appropriate weighted sum of
`y_i`.

If your `x_i` are an orthonormal basis for the input space, then you
can uniquely decompose `x` by `Xx`, where the rows of `X` are the
`x_i`. Note, importantly, that `Xx_i=e_i`. Since we want to use this
decomposition to predict the response value, we can do set `theta =
y\trans X`.

Let us say that the `x_i` are linearly independent and span the space
(i.e., they are a basis), but are not orthonormal. In that case the
decomposition `x' = X x` does not give us `x = X\trans x'`
exactly.

We assume there is indeed an `x'` such that `x = X\trans x'`. Then we
can consider `x'' = Xx = X X\trans x'`. So `x''` won't be right; it
won't be `x'`, and thus `Xx''` won't be `x`. So the answer is to
multiply by `(X X\trans)\inv`.

So the overall action on `x` is described by:

    (X X\trans)\inv X

Now, using this matrix, we can decompose `x` into the `x_i`. At that
point we need only apply `y`.

    y\hat = y\trans (X X\trans)\inv X x

Therefore, we can set:

    \theta = y\trans (X X\trans)\inv X

This says:

1. Project `x` onto the examples which are rows of `X`.
2. Oops. The examples weren't orthogonal. Fix the projection with
   `(X X\trans)\inv`.
3. Okay, now perform the interpolation! Take the appropriate weighted
   sum of the `y`.

**Does this Work?**

The main thing to check is that `(X X\trans)\inv` exists and that
`(X X\trans)\inv X` yields a proper decomposition. That is: if we set:

    x' = (X X\trans)\inv X x

Then:

    x = X\trans x'

Assuming `(X\trans X)\inv` exists, we already proved above that this
works. As ever, the matrix `X\trans X` means: map `x` to the skewed
version of `x` if we try to naively decompose into `x_i` and
reassemble. `(X\trans X)\inv` means: undo the skewing.

The question is whether `(X\trans X)\inv

This approach works if the following conditions are met:

1. 

In that case:

    x' = (X\trans X)\inv X x

We know that `X\trans X` is invertible. First, `X` is surjective
because, since the `x_i` are linearly independent, for each `x_i`
there is a vector not perpindicular to `x_i` but perpindicular to all
the rest. It is injective because since the `x_i` span the space,
there is no nullspace to `X`.

Likewise, `X\trans` has no nullspace, because the columns are
independent; therefore `X\trans` is injective. And, since the `x_i`
span, `X\trans` must be surjective.

Thus, we may set:


**When X doesn't span**

If the `x_i` don't span the input space, then `X` is not injective. In
that case, we cannot invert the transformation, since information is
lost in the process of projecting onto `X`.

Basically, the `x_i` don't specify the action on the whole input
space. We can avoid this problem if we could just ignore the part of
the input space not spanned by the `x_i`. It isn't illogical to try to
decompose `x` into the `x_i`; it's just that we can't quite use
`(X\trans X)\inv` to undo the skewing.

**When X is not linearly independent**

If the `x_i` are not linearly independent, then `X` cannot be
surjective. Let us say that `x_i` is a linearly combination of the
other basis vectors. In that case, if you tell me the projections of
`x` onto the other basis vectors, I can compute the projection onto
`x_i`. That means that `x_i` is determined by the others and cannot
take on just any value if the others are fixed.

In other words, if the `x_i` are not linearly independent, there is
more than one way to deconstruct a vector `x`, and we don't yet have a
way to choose here. In particular, there is more than one way to
deconstruct each of the `x_i`; we know for sure it isn't possible to
map each `x_i` to `e_i`, the number of `x_i` is greater than the
length of `x_i`.

This feels like a somewhat tougher problem, because we need to choose
which linear decomposition is "correct" for our purposes.

**Choosing A Decomposition**

We saw that if the `x_i` were numerous, there are many ways to
decompose `x` into a linear combination of the `x_i`.





Why do you want to decompose a vector `v` into rows of `A`? It's
probably because you have a `y_i` for each row `a_i`, and you want to
calculate `y_v`, which is the same linear combination of `y_i` as `v`
is of the `a_i`.

The obvious example is if `X` is a data matrix, where rows are
observations. Then you want to decompose a new `x` into the rows, so
that you can perform the appropriate *interpolation* by doing a
weighted sum of the response variables.

Now, your rows `x_i` are not orthogonal, so the reconstruction is not
quite right. That is, your `X\trans X x` decomposes `x` into
previously seen data points, then would reconstruct `x`. But because
of the skewing, your decomposition didn't work.

You can try to fix your reconstruction by applying `(X
X\trans)\inv`. Here's why. Say that `x'` is the proper deconstruction
of `x`. Then `x = (X\trans x')`. In that case, `X(X\trans x')` isn't
properly re-deconstructing `x` into `x'`, so it won't properly be
*reconstructed* into `x`.

A couple problems. If `X` doesn't span the space, then there never was
an `x'` that mapped to `x` anyway. That's not a major problem; the
application `Xx` just drops any information orthogonal to rows of `X`.

The bigger problem is if the rows `x_i` are not independent, in which
case `X\trans` is not injective. This is quite likely, because there
are probably more datapoints than there are dimensions.

In that case, the inverse of `X X\trans` cannot exist, because the
function is not injective.

Effectively this means: there are many ways to perform the
interpolation, and we don't know which one is correct.


This follows from the work we did before, we just have to view `\theta
-> y\hat` as a linear transformation defined by `X`. But let's try to
gain *even more* intuition.

Let's say that all `x_i` were orthonormal. Then the problem would be
exactly solved by saying "the gradient in direction `x_i` is exactly
`y_i`. But we can't have the `x_i` orthogonal because there are too
many of them.

**What is X\trans X Represent?**

I made this part up myself.

A good question is what is `X\trans X`. Then `X\trans X` is the matrix
of inner products of the `x_i` across the examples with the
`x_j`. This tells us how many units of `x_j` to predict for each unit
of `x_i` that we see. This is just another meaning of the projection
operation, and is the one-dimensional version of what we're trying to
do overall.

So this "mutual-information" (speaking loosely) of `x_i` and `x_j` is
exactly what we need to back-out. A simple model that projects `y`
onto `x_i` will double-count effects that are captured by other
variables. Thus we need to remove that with this matrix `X\trans X`.

Another note. This matrix `X\trans X` is like a mixing matrix that
mixes an orthogonal basis together. By inverting this matrix we can
undo the mixing. This is exactly what needs to be performed on
`X\trans Y`.

**TODO**: I think I could make this argument stronger if I knew more
about covariance. I think `X\trans X` might be related to the
empirical covariance. I am even more sure of that when I look at the
wiki page on estimation of covariance matrix.

**TODO**: I think this section is well intended: it is good to have an
intuitive understanding of what this projection of the data onto
itself means. But this isn't a comprehensive understanding yet; I
haven't really truly internalized this, or expressed this properly. I
should revisit this in time.

Finally, Strang notes that you can fit any kind of model, not just a
line, this way, so long as it is linear in a transform of each of the
predictor variables.
