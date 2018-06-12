## Purpose

We have a function `f(t)` which is expressed as a function of time. We
we want to "decompose" `f` into a weighted sum of sinusoidal functions
with different wavelengths.

This can certainly be done if `f` is a finite sum of sinusoidal
functions. We will have to discuss later what it means for a function to
be an "infinite sum" of sinusoidal functions.

## Relationship to Linear Algebra

Start with the set of all sinusoidal functions. These are
`sin(2*pi*fq*theta + phi)`. Equivalently we could say `sin(2*pi*f*theta +
phi)`; they're the same sets.

The vector space we're interested in is the closure of this set under
the operations of scaling and vector addition.

Assume that `f(t)` is in the set. What are we trying to do? We are
trying to decompose it into a weighted sum of orthogonal basis vectors.

What basis?

## Simplification: sinusoidals with period omega

Let's actually not consider *all* wavelengths. Let's fix a period
`omega` (or equivalently a frequency `fq`).

For a fixed `fq`, the space is parameterized by `phi` and `alpha`, where
`alpha` is the amplitude of the sinusoid:

  alpha sin(2*pi*fq*theta + phi)

That suggests this is a two dimensional space. But `phi` and `alpha`
aren't basis vectors.

## `sin` and `cos` are basis

Let's assume `omega = 2pi`.

I claim that `sin` and `cos` are a basis. What happens when you add
`alpha cos` and `beta sin` together?

Note that `(alpha, beta)` can be written as `r(cos phi, sin phi)`, for
some `r` and `phi`. Let's presume that `r = 1`.

Then we are asking, what is:

  cos(phi)cos(theta) + sin(phi)sin(theta)

This is the projection of `(cos(theta), sin(theta))` onto `(cos(phi),
sin(phi))`. Basically, you're projecting the `theta` vector onto the
`phi` vector, where the `phi` vector is `e_1` rotated up `phi` radians.
Which is to say: you're computing `cos(theta - phi)`.

This shows that `sin` and `cos` is exactly a basis of the space.

That makes sense. Whenever you have two independent vectors, a weighted
sum is going to effectively be (1) some scaling, and (2) some mixing.
And the mixing can always be seen as rotations.

Here there is a happy match-up because the basis vectors are themselves
sinusoids. And the rotation of sinusoids is equivalent to phase shifts.

## Multiplication of Complex Numbers

Let's do the same work with some slightly different notation. We start
with a refresh on complex numbers.

Complex numbers are all of the form `a + bi`. These are like vectors in
`R^2`. We know how to scale these by real numbers, and how to add these
together.

We know that every complex number can be written in polar form:
`r(cos(theta) + isin(theta))`.

How will we multiply complex numbers? The distributive law says FOIL,
but I want to try to give more intuition.

We know how to multiply `i` by `i`; the whole point is that `i^2 = -1`.
Note that multiply 1 by `i` is a rotation by 90deg, `i` multiplied by
`i` is a rotation by 90deg, et cetera. So this is giving us a hint.

I say that multiplying by `cos(theta) + isin(theta)` is equivalent to a
rotation by `theta` degrees. We know this is true when multiply `1` and
`i`.

I say: extend this definition of multiplication *as* rotation to all the
complex numbers. That is: multiplying `a + bi` by `cos(theta) +
isin(theta)` is defined to be the theta rotation of `a + bi`.

Rotation is a linear operation. If you break down a number and rotate
the parts and sum, it's the same as rotating the whole. Which basically
says: FOIL holds for rotation.

*Notice*: multiplying `c1` by `c2` is *not* equal to the length of the
projection of `c1` onto `c2`. I was previously confused about that one
time.

## Complex Exponential

We know that `e^x` is defined so that this function has itself as its
own derivative. Another way to say. `exp(alpha * x)` has derivative
`alpha * exp(alpha * x)`. If you interpret `alpha` as an interest rate,
that's how we talk about compounding interest.

`alpha` is telling you how to scale your current position to calculate
your velocity.

What about `exp(i*t)`? How will we choose to define that? Well, we want
our same property to hold. We want the velocity to equal `i * exp(i*t)`.

That says: we want velocity always to be perpindicular to the position,
with equal magnitude. We also know that `exp(0) = 1`.

That describes rotation around the unit circle with constant speed equal
to 1.0. A rotation takes `2pi` seconds. And the position at any time `t`
is given by:

  cos(t) + isin(t)

And that's what `exp(it)` will be defined as.

## Re-expressing Sinusoids with Complex Exponentials

Let's go back to our vector space of sinusoids. Let's talk in terms of
complex exponentials now. This will be our second language.

Note that `cos(t)` does not equal `exp(i*t)`, because it neglects the
imaginary part. So the space spanned by `exp(i*t)` has this other aspect
involved.

In a sense: `cos(t)` only cares about one scalar value, while `exp(it)`
incorporates both components.

Let's consider this as a vector space with *complex* scalars. So
multiplication of `exp(i*t)` by `-i` gives `sin(x) - icos(x)`.

What is the closure of this one-dimensional space? It is exactly those
functions which correspond as phase shifted versions of a
counter-clockwise rotation around a scaled circle.

The space does not include any functions that describe clockwise
rotation around a circle. Nor does it include any functions which don't
have both real and imaginary parts.

So our world with two basis vectors `sin` and `cos` with real scalars is
slightly different from our world with one basis vector `cos(theta) +
isin(theta)` with complex scalars. But they are also very similar.

## Projections of sinusoidals

Say we have `alpha sin(theta) + beta cos(theta)`, but we actually don't
know what `alpha` and `beta` are. How do we recover them?

This involves projection. What is an operation which performs
projection?

Integration of product is the obvious continuous analogue to the finite
dimensional projection operation.

Let's consider it. The integral of `sin` times `cos` over the period
`2pi` is the product of an odd and even function over a range equivalent
to `-pi, pi`. That means the integral must be exactly zero.

Good. What about projection of `cos` onto `cos`? `sin` onto `sin`?

Clearly `\int sin^2 + cos^2 = 2pi`. So that shows that projections must
each be `pi`, since they're phase shifted, and that doesn't change area.
Another way to see if that you're summing up all the `x` lengths of
every radii of the circle. But that's only 1/2 the length story; you're
leaving out the ys.

This shows that we need a *correction factor* when doing projections.
The correction factor is to multiply by `1/pi`. If the wavelength had
been `T`, that just stretches everything out by `T`, so the correction
factor would be `2/T`.

The projection operation is clearly linear, so after establishing that
it works on a basis it understands as orthogonal, we are done.

## Decomposition with complex exponentials

Let's try to think of the same operations but with complex exponential
notation.

Given `c exp(it)`, we want to recover the `c` value. This is like
asking: how much does `c` rotate `exp(it)` extra? How would we calculate
this?

The answer is to rotate *back* by multiplying by `exp(-it)`. this will
leave you with `c`. Of course, this works out algebraically, too,
because the exponents sum to zero, which means `exp(0) = 1`.

In our simple case we are considering here, there is no need to
integrate. If we did integrate on `[0, 2pi)`, we'd just get `2pi c`. You
can see the correction factor is `1/T`.

Why the difference? Maybe because the real valued approach of projecting
onto just `cos(theta)` only gives you half the story.

**TODO**: Can you make this `1/T` vs `2/T` story more precise?

## Sinusoids with period dividing T

What about a sum of sinusoids like this:

  cos(kt), sin(kt)

For `k` a non-zero positive integer? These are sinusoids which complete
a `k` periods over time `T`. They have frequencies equal to `k/T`. For
simplicity, I will continue to mostly consider `T=2pi`.

This is a generalization of what we were doing before. Note that now we
have the possibility of infinite sums. This brings in a topological
question. But let's stick just to the algebra and leave the topology
aside for now.

We want to decompose as before. The first question; is the basis I've
given even independent? Let's show that in a bit. I first just want to
talk about projections.

We know that `cos(kt)` projected onto `sin(kt)` continues to be zero.
What about `sin(kt)` projected onto `sin(kt)`? This is still `1/T`,
because you make `k` revolutions around, but you're making them `k`
times more rapidly. So the total remains the same and so does the
correction factor.

## Projections of sinusoids with different periods

The real question is: what about `sin(k1t)` onto `sin(k2t)`. Is the
projection zero? Here's how I see it. Say the first of these waves is
travelling faster than the second (one has to be if `k1 != k2`). Then
for every rotation that the first makes, the second falls `theta`
degrees behind.

So consider each time when the first wave is at phase `phi`. The
question is: what is the sum of the second wave's values at these times?

We know that these angles are all `theta` degrees apart, and that they
create an entire circuit. Let `theta` correspond to some complex `c`.
We're basically asking: what is `1 + c + c^2 .. + c^{k - 1}`, where
`c^k` would equal one again.

The answer is zero. Why? Well, you can see by multiplying everything by
`c`. The 1 becomes `c`, but note that `c^{k-1}` becomes 1. So you get
the same thing. What does that mean? It means, whatever the sum is, when
rotated, it stays the same. And unless `c=1`, that means that the sum
must be zero.

Inspiration: https://math.stackexchange.com/questions/891875/

## Period T Projections with Complex Exponentials

To project `f` onto `exp(i * k * t)`, we will do as before:

  1/2pi * \int f(t) * exp(-i * k * t)

As before, when `f(t) = c exp(i * k * t)`, we'll recover `c`. So
question is for any `f(t) c exp(i * k2 * t)`.

Remember, multiplication means rotate. So rotating back says: "how many
degrees is the `k2` wave ahead of me?"

Here's the point: when `k!=k2`, let's say `k2` is faster. Then at first
they are zero degrees apart. But then the angle is growing, until it
finally gets back to no angle ahead. This happens multiple times, but
by the end the vectors are in the same spot once again.

All this cancels out. Every moment we are `x` radians ahead is perfectly
balanced by another moment when we are `x` radians behind.

I think this view is more intuitive. It basically says: unless we are in
lockstep (`k=k1`) we will totally shift through each other, having a
net-zero correlation.

## Space of Functions Spanned

Any finite sum of sinusoidals will have period `T`. It will be
continuous and everywhere differentiable.

What about "infinite sums" of the basis vectors? Do these make any
sense? Maybe, if they converge, for some meaningful definition of
"converge."

The most natural topology to use is the L2 norm, which is the square
root of the inner product, which we have already defined. We could use
other norms or even more general topological definitions of convergence,
but this is not a topology course.

Let me give one justification for the L2 norm. The L2 norm basically
asks: how much of yourself would you want to use to reconstruct
yourself, according to this inner product? Say you have a sequence of
fns `f_k`, and the L2 norm of `||f - f_k||` doesn't converge to zero.
Then that basically says: I always want to use more of myself; the
approximation sequence hasn't fully captured what I am about.

Our space is not complete with respect to this basis. That is, there are
Cauchy sequences that don't converge to any algebraically spanned
function. I will later show that a sequence of sums converges to an
indicator function of `(a, b)`.

What is the completion of the space? It is all of the L2 integrable
functions on the period `T`. This should be provable if I can show (1)
the basis spans (topologically) any indicator `(a, b)`, and (2) any
function topologically spanned is square integrable.

So the completion is `L2(0, 1)`. By the time I'm done I should have
shown that the sine waves are a topological basis for this space. There
are other possible *algebraic* bases: ones which are linearly
independent, and can exactly sum to any function in `L2`. The problem
is: these would be uncountable in size, and thus useless, even if we
knew how to project on them. An algebraic basis is called a *Hamel
basis* whereas a topological basis is called a *Schauder basis*.

## Fourier Transform

We are also interested in *aperiodic* functions. That is: functions
defined on the whole real line.

An aperiodic function must use an infinite number of frequencies, as
otherwise it would be periodic on the longest wavelength.

Let's consider a single frequency `fq`. That is, we want to project onto
`exp(i * 2pi * fq * t)` by forming the integral:

    m_k(fq) = 1/(fq/k) \int_{-1/(2fq/k)}^{1/(2fq/k)} f(t) exp(-i * 2pi * fq * t) dt

Whew! The key thing is that this is `m_k(fq)`, where the base frequency
is `fq / k`, and the period integrated over is thus `k/fq`.

We know that if `f(f)` does in fact equal `exp(-i * 2pi * fq * t)`, then
this value `m_k(fq)` will *always* be 1.0. In fact, we can let `k`
continuously range toward infinity, and we will still converge to 1.0.
That's because the uncompleted part of the wave becomes less and less
significant, next to the many correctly measured complete cycles.

In general, for a fixed `k`, to reconstruct `f(t)` we use the `m_k(fq)`
as weights, and do the weighted sum out all frequencies a multiple of
`fq/k`. You could also call the weights *masses*.

But what if the mass is always zero? That can happen. As in: consider if
we split up a rectangle into finer and finer segments, under the theory
that we sum up the mass of all the segments to get back the original
mass.

But if you want to ask: how much mass is exactly at point `x`, the
answer must be zero. Your question was wrong: you want to know the
*density* at `x`.

So we want to break up these nonperiodic functions where frequencies
have zero mass at every specific frequency, but there is some concept of
density of energy at each point.

So how do we change our reconstruction method? The reconstruction was:

    \sum_{k=0}^\infty m_k(k*fq) exp(i * 2pi * k * fq * t)

Now the `m_k(fq)` has a leading term of `1/(fq/k)`. That happens to be
exactly the spacing of the frequencies that are detected for this `fq`
and this `k`. So you can treat that as a "width". And the leftover part,
which is:

    \int_{-1/(2fq/k)}^{1/(2fq/k)} f(t) exp(-i * 2pi * fq * t) dt

can be considered a *density*.

So this is showing that the limit of `m_k(fq)`, *without* the leading
correction term, is the correct density to use to get the reconstruction
method to work right.

Indeed, for the Fourier transform, we have:

    f\hat(fq) = \int_-\infty^\infty f(t) exp(-i * 2pi * fq * t) dt

with *no* correction factor. Likewise, the *inverse fourier transform*
is:

    f(t) = \int_-\infty^\infty f\hat(fq) exp(i * 2pi * fq * t) dfq

**TODO**: I can see the `1/T` as the interval width between frequencies,
and how that's like a volume, and how the integral part is converging to
a density. I see that because I know how we will invert the Fourier
transform, and I know the definition of a Riemann integral.

But I don't see *directly* why it is natural that dropping the `1/T`
factor leaves us with the density. As in: I don't really see what just
the integral quantity truly is.

## TODO

1. **TODO**: Prove that `L2(0, 2pi)` is topologically spanned by
   sinusoids with period dividing `2pi`.
2. TODO: Extend to entire continuum.
    * There's a hole in my understanding about dropping `1/T` to get the
      frequency density. I do understand how that gives us the right
      answer, but I don't understand why that quantity is natural.
    * So it's not a huge hole. But I'm a little unsatisfied...
3. TODO: Consider the DFT.
    * Why can we only detect frequencies up to `num_samples * duration`?

A. Complex version can handle just real valued version if you include
   counter rotation.

## Resources

This set of course notes is extremely useful. I copied it to my repo.

https://ocw.mit.edu/courses/nuclear-engineering/22-02-introduction-to-applied-nuclear-physics-spring-2012/readings/MIT22_02S12_read_fourier.pdf
