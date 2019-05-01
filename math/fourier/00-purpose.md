## Purpose

We have a function `f(t)` which is expressed as a function of time. We
we want to "decompose" `f` into a weighted sum of sinusoidal functions
with different wavelengths.

This can certainly be done if `f` is a finite sum of sinusoidal
functions. We will have to discuss later what it means for a function to
be an "infinite sum" of sinusoidal functions.

## Relationship to Linear Algebra

Start with the set of all sinusoidal functions. This is our "basis." The
sinusoidal functions are:

    sin(\omega * \theta + \phi)

The `\omega` means *angular frequency*. We have `\omega = 2\pi fq`. If
we measured just "regular" frequency, then a typical sinusoidal with a
period of `T = 2\pi` would have a frequency of `fq = 1 / 2\pi`. So
the multiplication by `2\pi` simply fixes things. We may also write:
`\omega = 2\pi / T`.

The basis of sine functions is sufficient to span cosines, as any cosine
function may be written as a phase-shifted sine function.

The vector space we're interested in is the closure of this set under
the operations of scaling and vector addition.

Assume that `f(t)` is in the set. What are we trying to do? We are
trying to decompose it into a weighted sum of orthogonal basis vectors.

We want not only finite linear combinations of basis vectors, but we
also want *infinite linear combinations*. We want to consider the basis
as a *topological basis*, rather than merely an *algebraic basis*. But
we'll consider that later...
