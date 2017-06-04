Okay. Our original setup was to decompoes a real valued function
cyclic with period `T` by:

    2/T \Int_{-T}^{T} f(x) sin(nx) dx

and

    2/T \Int_{-T}^{T} f(x) cos(nx) dx

We showed that this inner product makes all the `sin` and `cos`
orthogonal and of norm one. So if the function `f` is L2 integrable,
then the infinite decomposition will converge to *some* `f\hat`, and
the inner product of `f` and `f\hat` will approach zero. Of course, if
`f` is in the topological span of the basis, then we will converge to
exactly `f`.

Now, we may want to decompose some *complex* valued functions. Whoa!
In that case, we want to take the inner product with `cos(k*theta) +
isin(k*theta)` and likewise for `sin(k*theta) + icos(k*theta)`. Or is
that what we want?

First, I want to note that we only need one of these sets of basis
vectors. Note that `sin(k*theta) + icos(k*theta)` is, in the
beginning, equal to `i * [cos(k*theta) + isin(k*theta)]`. That's
because multiplying by `i` just flips the basis vectors.

But that isn't enough, because the product I gave is equal to
`-sin(k*theta) + icos(k*theta)`: this corresponds to a circle spinning
counter clockwise starting at `i`. Basically, I've just translated the
original circle by `pi/2` radians.

On the other hand, the original `sin(k*theta) + icos(k*theta)` wants
you to spin *clockwise*. But the fix is easy: just use `-k`.

We can summarize. You used to need to have both `cos` and `sin` in
your basis so that you could use both to get a phase shift. But now
that you can use a complex weight, you can use that to get the phase
shift, not just the magnitude.

Of course, in exponentional form:

    e^{i * k * theta} = cos(k * theta) + i * sin(k * theta)

So we can use these as our basis vectors.

Onward to the second problem! It is this: our inner product is broken!
Let's show that *reversed* periods are not *orthogonal* like they
should be:

    \Int_{T/2}^{T/2} (e^{i * theta})(e^{i * -theta}) d theta
    = \Int (cos(theta) + isin(theta))(cos(-theta) + isin(-theta)) d theta
    = \Int (cos + isin)(cos - isin) d theta
    = \Int (cos^2 + sin^2) + i(sin*cos -sin*cos) d theta
    = \Int 1 + 0 = T

What the heck? Why is this suddenly a problem? Well, first off, when
we were just dealing with real valued functions, we don't want to
consider negative periods. Because `sin(-theta) = -sin(theta)` and
`cos(-theta) = cos(theta)`. So if we allowed negative periods, we
would be "duplicating" existing basis vectors.

We now want to allow this because there is a difference between
`cos(theta) + isin(theta)` and `cos(-theta) + isin(-theta)`. These are
now independent.

What went wrong? It used to work so great! We basically were doing the
limit of a series of projections onto finite dimensional
approximations.

The problem is that `z_1 * z_2` is *not* the projection of `z_1` onto
`z_2`. Multiplication of complex numbers is not the same as the 2d dot
product. The problem is the cross terms:

    (a + bi)(c + di) = (ac - bd) + i(ad + bc)

First, this is complex valued, so that's new. And the formula looks
not quite right. Let's explore with some `sin` and `cos` in there:

    r_1(cos(theta_1) + isin(theta_1)) * r_2(cos(theta_2) + isin(theta_2))
    = (r_1*r_2) * [
        (cos(theta_1)cos(theta_2) - sin(theta_1)sin(theta_2))
        + i * (sin(theta_1)cos(theta_2) + cos(theta_1)sin(theta_2))
    ]

The `r_1*r_2` part sounds like the magnitude want. Now, if we want
just the normal projection, we want to focus on the middle part of the
product, which is wrong.

Look. The right answer is `cos(theta_2 - theta_1)`. Now, you can do a
trig identity to see that what we have in that second part of the
product is `cos(theta_2 + theta_1)`. That's exactly wrong!

Now, we're working with complex numbers, so we want to retain
information about the amount *orthogonal* in the imaginary part. That
means we want `sin(theta_2 - theta_1)`. But again, we have exactly the
wrong thing. Here, instead of the sum, we want the difference.

The fix is to multiply by *the conjugate*. Here's the idea. If you
have two unit vectors, then the complex projection is just the vector
that describes the rotation to go from `u_1` to `u_2`. That is
achieved by performing the "undo" of `u_1` to `u_2`. The undo is the
reciprocal, we is the conjugate. Basically, if `u_1` is a rotation by
theta, this performs a rotation by `-theta`.

If we don't have unit vectors, that's fine. The scales ought to
multiply as they do.

This shows that the good inner product is:

    1/T \Int_{T/2}^{T/2} f(theta) conj(g(theta)) d theta

Now, we may note that we use `1/T` here instead of `2/T`. This is
because with the complex valued Fourier transform, we are capturing
both dimensions of change: `e_1` and `e_2`. If `f = e^{ix}`, then this
maps out the unit circle. Since `u \cdot u = 1` for any unit `u`, then
this is `\Int_{-\pi}^{pi} 1 = 2pi`.

Let's consider if `f(x) = cos(x)`, a *real-valued* function. Then
integrating `cos(x) * conj(e^{ix})` gives you `pi` since this integral
is equal to `(\Int cos(x)cos(x)) + (\Int -isin(x)cos(x))`. So we use
1/2 of `e^{ix}`. But then the same math works for `e^{-ix}`, so we use
1/2 of that as well. The sum of those is just `cos(x)`, because the
action in the imaginary dimension is canceled out.

This shows that the complex fourier transform is just the real valued
version, too.

Putting this all together, we have established that to find the amount
of a wavelength `2pi/omega`, we want to calculate:

    1/T * \Int_{-T/2}^{T/2} f(x) * conj(e^{i*q*x}) dx
    = \Int_{-T/2}^{T/2} f(x) * e^{-i*q*x) dx

Note how we put the negative sign in, because we need the conjugate.

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
