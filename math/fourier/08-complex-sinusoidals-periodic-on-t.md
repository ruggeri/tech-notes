Let's again move from the one-dimensional space of period `T=2\pi`
sinusoidals, to the space spanned by the basis:

    \exp{i * k * t} (k is an integer; positive or negative)

I want to note that, unlike when we considered real valued sinusoidals,
it matters if we allow negative `k`. Using negative `k` will run the
sinusoidal backward. We didn't need to do that previously, because
`cos(-t) = cos(t)`, and `sin(-t) = -sin(t)`, which is a scalar multiple
of `sin(t)`. But with our complex sinusoidals, there is no `c` such
that:

  c \exp{i * k * t)} = \exp{i * -k * t}

**Inner Product Continues To Work Great**

To project `f` onto `exp(i * k * t)`, we will do as before:

    1/2pi * \int f(t) * exp(-i * k * t) dt

As before, when `f(t) = c exp(i * k * t)`, we'll recover `c`. So we may
assume instead that `f(t) = c exp(i * k2 * t)`, for `k2 != k`.

The story is the same as we considered before for real vector space
spanned by `{ cos(kt), sin(kt) | k}`. But I will give a second story,
more specific to our new representation.

Remember, multiplication means rotate. So rotating back says: "how many
degrees is the `k2` wave ahead of me?" We are basically integrating to
see the "average" number of degrees by which `f` is ahead of
`exp(i*k*t)`.

Let's say `k2` is greater than `k`. Then at first both `f` and
`exp(i*k*t)` are zero degrees apart. But then `f` is gaining ground,
getting farther and farther ahead.

Finally `f` gets back to no angle ahead. If `gcd(k, k2) > 1`, then they
will make additional "full" rotations like this, but it won't matter
(since the integral on the first "full" rotation will be zero anyway).

The point is: all this will cancel out. `f exp(-i*k*t)` is itself
describing circular motion. And the integral on circular motion is zero.
Everything will cancel out.

I think this view is more intuitive. It basically says: unless we are in
lockstep (`k=k1`) we will totally shift through each other, having a
net-zero correlation.
