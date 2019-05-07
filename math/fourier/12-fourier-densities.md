**We Need A Metric On The Basis To Do Integration**

Once we have factored out the "mass" part, we can start talking about
*density* at each `\omega`. That will increase the number of functions
that are decomposable. But in order to have a notion of density, we must
have a notion of *locality* (topology). More than that! We need a notion
of *metric* (or maybe just *measure*?) on the space `exp(i*\omega*t)`.

Note that if we want to find "spectral density," we cannot use a metric
implied by the inner product we have been using up to now. Why?

The reason is that the Fourier basis is *discrete* with respect to the
norm I've been using. You might suspect that this is the case, because
the inner product of two basis vectors is always `0.0`. If you do the
math (and I have at the whiteboard), you'll see that the L2 "norm" from
the previous section evaluates to `\sqrt{2}` for any two sinusoidals
with different periods. That makes sense: `||e_2 - e_1||_2 = \sqrt{2}`
sounds like it should be correct.

So let's introduce our own metric on the basis space. We will measure
distance between two sinusoidals by the distance in their angular
frequency. Given that, we know what we want:

    f(t) = \int f\hat(\omega) exp(i*\omega*t) d\omega

(I have not included a normalizing coefficient here yet...)

**Exploring Relative Mass**

What is density? I say: it is the limit of the *relative* mass at points
`x, y` when we consider equal-diameter `\eps` balls around those two
points. We should add: the "relative masses" should be normalized by a
constant so that the total mass, when integrated out, will equal the
desired mass.

Let's consider our traditional mass calculation:

    \int_T f(t) exp(-i \omega_0 t) / |exp(-i \omega_0 t)|^2 dt

We know this must converge to zero, by the fact that `f` has finite L2
norm. This is the *correct* mass.

*But*, what if, at the same time I was expanding the range of
integration, I was also applying a multiplier that multiplied `f` by:

    \int_T |exp(-i \omega_0 t)|^2 = T

Doing such a multiplication by a constant should not change the
*relative* masses of the various frequencies, since it changes
everything equally.

Let us say that this calculation of relative masses converges. God
willing, we will also have that

    \int f\hat(\omega) exp(i*\omega*t) d\omega

converges for all `t`. (There are various technical requirements on `f`
for this to be true.)

If both those quantities converge, then hooray! We now need only
normalize the result appropriately.

**Can The Relative Masses Be Normalized?**

A more basic question: we've been calculating these "relative masses"
for different `\omega`, but what we were doing didn't seem to consider
the metric we will use when integrating over all angular frequencies. If
we were integrating with a different metric, then would these relative
masses be correct?

I would say this. For any value `T`, there are a number of frequencies
that correspond to the Fourier series if we had restricted the function
`f` to period `T`. Those frequencies are evenly spread out in `\omega`;
they are all products of a base frequency `1/T`. Which means that, if we
give each of those frequency amplitudes an equal weight equal to a width
of `1/T`, then (when properly normalized overall), that should converge
to the correct answer as `T` increases.

**What is the normalizer?**

The normalizer pertains to the difference between measuring time in
seconds, but measuring frequency in terms of radians per second rather
than cycles per second.

Consider trying to project `f(t) = exp(i*t)` onto itself. Consider
calculating this projection over one cycle: from zero to `2\pi` seconds.

The "normalized" projection is:

    \int_0^{2\pi} f(t) exp(-it) / |exp(it)|^2 dt
    = \int_0^{2\pi} 1.0 / 1.0 dt
    = 2\pi

I want to compare this to the corresponding "granularity" of projecting
over `2\pi` seconds. The Fourier series would ask us to assign a value
to each of `exp(i*k*t)`, which is an angular frequency width of `1.0`.

By increasing `T` to `2k\pi`, we reduce the angular frequency width to
`1/k`, while increase the total "mass" of the projection to `2k\pi`. To
me, this means that the mass assigned to a region is always `2\pi` times
greater than desired. Thus we should apply a correction factor when
performing the inverse Fourier transform:

    \frac{1}{2\pi} \int f\hat(\omega) exp(i*\omega*t) dt

**Other Notations**

Physicists seem to prefer angular frequency. They can gain a symmetric
Fourier transform/inverse Fourier transform by applying a correction of
`\frac{1}{\sqrt{2\pi}}` on either side.

Another alternative is to project onto `exp(i * 2pi * fq * t)`. Then
then unit of frequency does match up properly!
