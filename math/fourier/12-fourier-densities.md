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

(This argument is unclear, but I  expand and clarify below.)

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

We know that the mass will scale linearly in `k`, the number of complete
rotations; that is `2k\pi` radians. But we can see that the mass being
calculated is always off by a factor of `2\pi`, even if you took care of
eliminating the factor by which increased `T` results in increased
projection magnitude (which we have done, by taking the ratio of
projections).

Here is another thought. What is the correct density? We know that, in
the limit of `T`, the result for `exp(i*t)` is unbounded. But if
`T=2\pi`, then the Fourier series would ask us to assign a value to each
of `exp(i*k*t)`, which is an angular frequency width of `1.0`. And the
mass to assign will always be `1.0` (if we've normalized correctly).

By increasing `T` to `2k\pi`, we *reduce* the angular frequency width in
the Fourier series to `1/k`. But the desired mass to assign is *still*
`1.0`. So the correct density for the interval of width `1/k` is `k`.

But the integral is giving us back a value of `2k\pi`, which is exactly
`2\pi` too much.

Thus we should apply a correction factor when performing the inverse
Fourier transform:

    \frac{1}{2\pi} \int f\hat(\omega) exp(i*\omega*t) dt

**Other Notations**

Physicists seem to prefer angular frequency. They can gain a symmetric
Fourier transform/inverse Fourier transform by applying a correction of
`\frac{1}{\sqrt{2\pi}}` on either side.

Another alternative is to project onto `exp(i * 2pi * fq * t)`. Then
then unit of frequency does match up properly!
