## Fourier Transform

We are also interested in *aperiodic* functions. That is: functions
defined on the whole real line.

An aperiodic function must use an infinite number of frequencies, as
otherwise. If it were comprised of a finite series of sinusoidals, each
with period `\lambda_i`, then just take `\lambda = \Pi_i \lambda_i`. The
function has period dividing `\lambda`.

Let's project our aperiodic function `f` onto a sinusoidal with
frequency `fq`.

Let's consider a single frequency `fq`.

That is, we want to project onto
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
