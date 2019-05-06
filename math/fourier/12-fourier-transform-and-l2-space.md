Let me propose that:

    f\hat(\omega) = \int f(t) exp(i*\omega*t) dt

If that is so, let me consider

    \int f(t) exp(-i*\omega_0*t) dt

I would like this to be equal to:

    \int
      (\int f\hat(\omega) exp(i*\omega*t) d\omega)
      exp(-i*\omega_0*t) dt

Why? Because this is very similar to the projection of `f` onto
`exp(i*\omega_0*t)`, except that we have not done the required
normalization. That is: the mass of the projection of `f` onto the basis
vector is really zero.

But, if we ignore the normalization, we still get quantities that are
the right *relative values* for different `\omega_0, \omega_1`.
