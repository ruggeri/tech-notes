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

Our basis vectors aren't exactly unit vectors. So we can always fix
things by normalizing them: `{ \sqrt{2/T} sin(\theta), \sqrt{2/T}
cos(theta) }`. I'll often leave out the correction factor in these
notes, but it's no big deal.

The projection operation is clearly linear, so after establishing that
it works on a basis it understands as orthogonal, we are done. We know
it works for decomposing linear combinations of the basis vectors.

## L2 Inner Product Intuition

Let me try to give some intuitive pictures for why the integral inner
product is natural.

Recall that the standard inner product in `R^n` (the dot product) is
correct if you want to decompose a vector `u` into (1) a component along
`v`, and (2) a component in the rank `n-1` space "orthogonal" to `v`.
This "orthogonal" notion presupposes the concept of rotation of basis
vectors.

We can see functions on `R` as an extension to this. Especially if we
restrict to functions that are the (pointwise?) convergence of step
functions.

One could say: in `R^n` we had the notion that we could always "keep
rotating" and eventually end up back at the starting vector. So that
suggests that our basis vectors ought to be all "translates" of each
other, with respect to rotation. But that is also a little odd: in `R^n`
there are `n-1` angles that define a unit vector, whereas if we choose
the Fourier basis for functions on `R` there is only one continuous
variable of rotation...

A "statistics" view is that you are asking for the "correlation" between
`u` and `v`. In that setting, the inner product makes sense.

**TODO**: clarify these random, still un-unified and incomplete thoughts
into a more coherent story.
