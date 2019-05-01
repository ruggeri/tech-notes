## Resolution of accuracy

There is a note that I have neglected! `e^{i*k_1*x}` and `e^{i*k_2*x}`
are only orthogonal when integrate over a range `[-A, A]` where `A` is
evenly divisible by both `2pi/k_1` and `2pi/k_2`. That is: we need to
integrate over a whole number of cycles for *both* sinusoidals.

If we work over an interval `[-T/2, T/2]`, then we can only identify
frequency amplitudes for sinusoidals with period evenly dividing
`T`.

Let us say we want to measure concert A and concert B. These are
frequencies 440hz and 493.88hz. Now, if I have 1/55 seconds of
samples, that gives enough time for 8 turns of a 440hz frequency and 9
turns of a 495hz frequency. So I can find how much A and how much B
are in this signal, assuming that all other frequencies in the signal
have wavelength dividing this.

Note that if this is not true, I'll find non-zero amplitudes for an
infinite number of frequencies, which when summed turn out to be an
approximation that approaches the frequency that doesn't divide
properly.

So like say you have 1 second of data. That means you can capture any
integer frequency. You don't accurately capture frequencies less than
1hz or any non-integer frequency like pi hertz.

If you have 0.25sec of data, you can capture any signal at least 4hz,
and have a resolution of 4hz.

## Over arbitrary horizons?

Note that I was worried about the correction factor `1/T`. You can use
the same correction factor for any sinusoid with wavelength `T/k`,
because the integral over one *period* of length `T/k` is 1 times the
*length of the period*. So this is `T/k`. There are then `k` of these,
so you get 1.0.

What if you have a signal over an infinite duration? The problem is
going to be that if a periodic function doesn't have zero norm on its
period, then it will have *infinite* norm over the infinite horizon.

Of course, we can always consider a very long interval, and not the
entire interval, and then get an arbitrarily high resolution
approximation.

Alternatively, if the function is not really periodic, but has finite
norm over the entire space, I believe we can still decompose it using
the Fourier transform. That means that outside a finite interval, the
norm of `f` is `<epsilon`. So you could pick a big enough interval and
do your normal approximate decomposition.

Alternatively, I believe you might be able to do a decomposition
integrating over the entire space. Now, in that case, you could have
non-zero amplitudes for *every* real frequency, but that these
eventually get very small, to the point where the norm of theh
function defined by the extremely low frequencies goes to zero.

Actually, I think it goes to the average, maybe.
