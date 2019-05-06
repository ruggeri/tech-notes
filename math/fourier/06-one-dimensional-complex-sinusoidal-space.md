Let's go back to our vector space of sinusoids with fixed period
`T=2\pi`. Let's talk in terms of complex exponentials now.

Note that `cos(t)` does not equal `exp(i*t)`, because it neglects the
imaginary part. So the space spanned by `exp(i*t)` *does not* contain
simply the real functions `sin(t)` nor `cos(t)`.

In a sense: `cos(t)` only cares about one scalar value, while `exp(it)`
incorporates both components.

Let's consider this as a one-dimensional vector space with *complex*
scalars. So multiplication of `exp(i*t)` by `-i` gives `sin(x) -
icos(x)`, for instance.

What is the closure of this one-dimensional space? It is exactly those
functions which correspond as phase shifted versions of a
counter-clockwise rotation around a scaled circle. Isn't that easy to
see, since we know what complex multiplication means?

The space does not include any functions that describe clockwise
rotation around a circle. Nor does it include any functions which don't
have both real and imaginary parts.

So our world with two basis vectors `sin` and `cos` with real scalars is
slightly different from our world with one basis vector `cos(theta) +
isin(theta)` with complex scalars. But they are also very similar.
