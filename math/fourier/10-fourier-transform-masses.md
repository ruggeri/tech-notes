We are also interested in *aperiodic* functions (in addition to periodic
ones). That is: functions defined on the whole real line. We want to
decompose them into the *Fourier basis*, which is:

    exp(i * \omega * t)

Where `\omega` is a real valued angular frequency.

An aperiodic function must use an infinite number of frequencies. If it
were comprised of a finite series of sinusoidals, each with period
`\lambda_i`, then just take `\lambda = \Pi_i \lambda_i`. The function
has period dividing `\lambda`.

Now here is a new interesting scenario. This Fourier basis is an
*uncountable basis*. This is very interesting. Maybe we can still focus
on finite or even countable sums of the basis vectors to start with,
though. (Note, a countable linear combination of basis vectors can be
aperiodic.)

What would be our generalization of the inner product? We could try:

    \lim_{T->\infty}
      \frac{
        \int_{-T/2}^{T/2} f(t) \conj{g(t)} dt
      }{
        \int_{-T/2}^{T/2} g(t) \conj{g(t)} dt
      }

Note that this is a "normalized" version of the L2 inner product. The
reason we want to do this normalization is because `exp(i*\omega*t)` has
infinite norm.

If `f(t) = g(t) = exp(i*\omega*t)`, then this continues to be `1.0` as
expected. Why? Whenever `T = k \omega`, then this is perfectly `1.0`.
The "error" of accounting only for a part of the full period is greatest
at some points `T = (k + eps) \omega`. But the magnitude of the error
doesn't increase as `k` increases, while the denominator's magnitude
(`T`) does. That is: the error from one last incomplete period becomes
much less important than the value of integrating over a number of
"correct" cycles.

What about when $f(t) = exp(i*\omega_1*t)$ but $g(t) =
exp(i*\omega_2*t)$? We already know that answer. Consider $T = k *
(\omega_1*\omega_2)$. We showed previously that the inner product over
such a `T` is zero. Again, we have the same "error" analysis, so again
the limit approaches zero.

Calculating these limits, if `f` is indeed equal to a countable sum of
the basis vectors, we may decompose `f` into "mass" values for each
basis vector.

(Technically I didn't show convergence of the sum in the L2 norm, but
whatever...)
