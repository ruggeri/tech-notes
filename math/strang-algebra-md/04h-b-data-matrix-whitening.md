I've already told you about how to fit a linear least squares model:

    \theta\hat = (X\trans X)\inv X\trans Y

The reason we need `(X\trans X)\inv` is because columns of `X` are not
necessarily orthogonal, and thus `X\trans` doesn't properly invert
`Y`. The reason is because of the "correlations" and "double-counting."

You can view `X\trans X` as a mapping from "original coordinate
system" (in terms of columns of `X`) to "mixed up coordinate system".

Of course, if `X` didn't do any mixing up, then `X\trans Y` would be
just fine. You can just project onto each column of `X` independently.

So another approach might be: can we transform `X` itself so that
`X\trans Y` simply works?

We can. We will want to find a *whitening matrix* `W` such that `XW`
has unit variance and zero covariances. Note: `W` is num-features by
num-features; it is a full rank transformation of the feature space.

Great, so what can `W` be? Well, we must have:

    (XW)\trans XW = I
    W\trans (X\trans X) W = I
    W W\trans (X\trans X) W = W
    (W W\trans) (X\trans X) I = I
    (W W\trans) = (X\trans X)\inv

Okay, well `W W\trans` looks a little weird. But if we assume `W` is
symmetric, then no worries.

There are many choices for `W`, but I'll show PCA whitening. We'll
later see that any real symmetric matrix can be written as:

    X\trans X = Q D Q\trans

This is the eigen-decomposition, and the spectral theorem says you
have to have one if you are real symmetric.

Therefore, we may set `W = Q D^{-1/2} Q\trans`. This basically says,
shrink the eigenvectors of `X\trans X` by the appropriate amount.

In that case, we have:

    W W\trans = W\trans W (because W symmetric)
    = (Q D^{-1/2} Q\trans) (Q D^{-1/2} Q\trans)
    = Q D^{-1} Q\trans

Great!

I guess whitening isn't so bad: we need to keep `n` angles (for the
`Q` matrix) and `D` diagonal coefficients. That's `O(n)`
information. We can then transform any incoming `x` values pretty
easily.

Note that in this world, we are projecting `Y` onto `XW` to get our
`\theta`. We then write:

    (XW) \theta = Y\hat

But let's break this out:

    XW ((XW)\trans Y)
    X (W\trans W) (X\trans Y)

And we know that `W\trans W = (X\trans X)\inv`. So we are really just
back where we started. Instead of keeping `W` around in any form, we
can just do `W \theta` now and we can use this like an original
`\theta` for the unwhitened `X`.

## ZCA vs PCA Whitening

There's an Arxiv paper called "Optimal Whitening and
Decorrelation". It explains that my choice of `W` is called *ZCA
whitening*.

An alternative is simply `W = Q D^{-1/2}`. Note that we still have:

    W W\trans = Q D^{-1/2} D^{-1/2} Q\trans = Q D^{-1} Q\trans

**TODO**: Maybe explore why `W W\trans` instead of `W\trans W`? The
first is correlation in rows vs the second is correlation in columns?
They're not the same if `W` is not symmetric (as in PCA whitening).

**TODO**: Any geometric understanding of the SVD of `X\trans X` to
help us understand what is going on here?
