Say we have `alpha sin(theta) + beta cos(theta)`, but we actually don't
know what `alpha` and `beta` are. How do we recover them?

This asks for a projection. What is an operation which performs
projection?

Integration of product is the obvious continuous analogue to the finite
dimensional projection operation.

Before we get to deep into considering why that is a good inner product,
let's simply show that *it works*.

First, are the basis vectors orthogonal?

The integral of `sin` times `cos` over the period `2pi` is the product
of an odd and even function over a range equivalent to `-pi, pi`. That
means the integral must be exactly zero.

Great. Are the basis vectors unit vectors?

Clearly `\int sin^2 + cos^2 = 2pi`. So that shows that projections must
each be `pi`, since they're phase shifted, and that doesn't change area.
Another way to see that `\int sin^2 = \pi` is to consider summing up all
the `x` lengths of every radii of the circle. But that's only 1/2 the
length story; you're leaving out the ys.

So our inner product is a little off; we need a correction factor for
doing projections properly. That correction factor is to multiply by
`2/T`, which in this case is `2/2pi = 1/pi`.

(Note that no correction factor would have been needed if the period was
simply `1`.)

The projection operation is clearly linear, so after establishing that
it works on a basis it understands as orthogonal, we are done. We know
it works for decomposing linear combinations of the basis vectors.

**TODO**: Could I give a more complete story about why this is the inner
product?
