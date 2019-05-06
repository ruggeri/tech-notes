We would like to consider *uncountable sums* of our basis vectors. What
could that even mean?

It's clear we want to do some kind of integration. We want a *measure*
on the space of `exp(i*\omega*t)`. The measure will assign *masses* to
*sets* that partition the space. We already saw that some points in the
space may be assigned some *mass*.

Let me call attention to functions with finite L2 norm that have compact
support (these are aperiodic by definition). Then note that the mass for
each frequency `\omega` in the decomposition must be zero.

Once we have factored out the "mass" part, we can start talking about
*density* at each `\omega`. That will increase the number of functions
that are decomposable. But in order to have a notion of density, we must
have a corresponding metric on the space `exp(i*\omega*t)`.

This metric *cannot* be implied by the inner product we have been using
up to now. Why?

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

    f(t) = \int_\omega f\hat(\omega) exp(i*\omega*t) d\omega
