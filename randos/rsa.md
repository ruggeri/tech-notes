## Basic Idea

The idea is to choose three numbers:

1. Encryption key `e`.
2. Decryption key `d`.
3. Modulus `n`.

Such that:

    (m**e)**d = m (mod n)

## Choosing `p, q, n`

You choose big primes `p` and `q`. These can be randomly sampled, and
then you use Fermat's theorem to do primality testing (basically check
that `a**p = a mod p`. After that you can do Miller-Rabin (I don't know
how that is done) to get even more assurance. They also "preselect"
numbers by filtering out all those divided by common small prime
factors.

You then let your modulus `n=pq`.

## `phi(nq)`

We know `phi(n) = (p - 1) (q-1)`. Phi is Euler's totient function: the
number of coprime numbers `< n`.

Let's consider numbers that are *not* coprime to `n`. Then they must be
either:

    1p, 2p, ..., (q-1)p
    1q, 2q, ..., (p-1)q
    pq

Let's think about this. First, if we cancel out all the multiples of `p`
(including `pq`), then we have `p(q-1)` numbers. But some of these are
multiples of `q`. There are `q-1` of them (because we should not include
`pq`; already done that). Subtracting out `q-1` gives `(p-1)(q-1)`.

## Euler's theorem

Euler's theorem says:

    a ** phi(n) = 1 (mod n)

When `n` is prime, then `phi(n) = n-1`. This is just Fermat's little
theorem.

Therefore, if `ed = 1 mod phi(n)`, we have:

    (m**e)**d = m (mod n)

Therefore, we would like that `ed = 1 mod (p - 1) (q - 1)`.

## Choosing `e`

First, choose any `e` coprime to `(p - 1) (q - 1)`. `e = 3` is a common
choice for fast encryption. You could just start over if `e = 3` is a
factor. `e = 65,537` is also a common choice.

## Finding `d`

We must next choose `d` to be the inverse of `e` mod `(p - 1) (q - 1)`.

How do we invert `e`? This is an application of the extended Euclidean
algorithm. I describe it in another document. It's easy to do.

## RSA Security

It is not necessary to remember `p, q`. We only need `(e, n)` and `(d,
n)`.

If we know how to factor `n`, we know how to calculate `phi(n)`, and it
is easy to invert `e mod phi(n)`. But if we don't know how to factor
`n`, then we don't know how to calculate `phi(n)`. In that case, we
don't know what to invert `e` with respect to.

Important point: the following is *not* true:

    g^{x} mod a, AND y = x mod a THEN
    g^{x} = g^{y}. WRONG

Basically, mods don't work in exponents. Here's an example (`a = 123`):

    101 ^ 8 % 123 = 109
    101 ^ {246 + 8} % 123 = 32

Does finding `d` mean that you must be able to factor `n`? I believe
this is unknown. Therefore breaking the private key of RSA may be easier
than factoring.

## Notes

Euler's `phi(n)` is not always the least number where `m**x = m mod n`
for all `m` values. That value is called Carmichael's lambda function.
It is smaller and more specific. But we don't have to discuss this.
