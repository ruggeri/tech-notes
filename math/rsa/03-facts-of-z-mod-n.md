Let's now consider the finite ring `Z mod n`, where `n` need not be
prime. We will contrast with the scenario of `Z mod p`.

**Some non-zero elements will multiply to zero**

Since `n` is not prime, there exist `a, b` (possibly equal) such that `n
= ab`. Then we have that:

    ab = 0 (mod n)

**Some non-zero elements won't cancel**

Note that:

    a 1 = a (b + 1) (mod n)

This can happen even though `1 \ne b + 1`. Therefore it is not safe to
cancel `a`.

**Some non-zero elements aren't invertible**

Since two products `ak_0 = ak_1`, it is not possible for an inverse of
`a` to exist.

**`a` is invertible iff `gcd(a, n)`**

Consider an element `a` such that `gcd(a, n) = 1`. Then for any `b \ne
0` in `Z mod n`, we know that:

    ba \ne 0 (mod n)

The reason is: how can `ba` be a multiple of `n`? It must contain all
prime factors of `n` (with the correct multiplicity). If `a` is
relatively prime to `n`, then we must have that `b = a`.

Note that we have already previously explained (when talking about `Z
mod p`) that when `ba` never is equal to zero, it must be that:

    1a, 2a, ... (n - 1)a

is a permutation of `1, 2, ..., n - 1`, which means that `a` invertible.

Alternatively, consider any `a` such that `gcd(a, n) \ne 1`. Then
consider `b = n / gcd(a, n)`. Note that `0 < b < n`. We now have:

    ba = n = 0 (mod n)

Of course this immediately implies that `a` is *not* invertible.

**The multiplicative subgroup of `Z mod n`**

The *multiplicative subgroup* of `Z mod n` consists of all the
invertible elements of the ring. We've just said that this consists of
all elements `a` where `gcd(a, n) = 1`. That is: when `a` is relatively
prime to `n`.
