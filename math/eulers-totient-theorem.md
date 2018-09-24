## Euler's Totient Theorem

Euler's totient theorem says:

    a^{phi(n)} = 1 (mod n)

Here `phi(n)` is the number of coprime numbers `<=n`. Note of course
that `phi(p) = p-1`. So this generalizes Fermat's little theorem.

## Not every element is invertable mod `n`

Every element must eventually cycle back to itself or to zero, if
continuously exponentiated. If `n=pq`, then `x**i` can never be
divisble by `n`. But if `n=p**2` then `x**i` can end up at zero.

Regardless, even if `x**i` cycles back to `x`, this does *not* mean
`x` is invertable. Indeed, consider any `x` dividing `n`. Then we
have:

    xk = 0

So if we were able to invert `x`, we could multiply by `x**-1` to get:

    k = 0

Note that no element `x` not coprime with `n` is invertable. Since the
"missing" factors of `n` from `x` are `<n`.

## The multiplicative subgroup of Z mod n

Let's just consider the coprime numbers. They form a subgroup within
`Z mod n`. There are `phi(n)` of these elements.

We may now consider cyclic subgroups of this multiplicative group. 

We can prove Euler's totient theorem just like we proved Fermat's
little theorem with Lagrange. We know by Lagrange that the size of any
subgroup must divide the size of the whole group. So this gives us
Euler's theorem right away as a corrolary.

## Factoring chains

Let's take another approach. Let's form all the chains of the
multiplicative group. We can factor these down to chains of order
`p**k`.

I believe that crossing chains of coprime order still gives a chain
with length equal to the product. Let's think:

1. All elements of `a**i, b**j` have coprime order. Since it is clear
   that order of an element must divide the size of the cyclic
   group. (This property is a little more obvious than Lagrange).
2. This implies that `a**i` and `b**j` can never be inverses. Since
   they still must have equivalent order.
3. This means that `a**i b**i = 1` only when both `a**i` and `b**i`
   are 1. Which only happens for `p_1**k_1 p_2**k_2`.

So if we break down to the chains of length `p**k`, I want to multiply
these together to form a chain of length `phi(n)`.

## Carmichael

Apparently there is a Carmichael lambda function. Sometimes there is a
smaller number `lambda(n) < phi(n)` where:

    a**lambda(n) = a mod n

for all `a`. This implies that sometimes the multiplicative group is
*not* cyclic (no element has order `phi(n)`). That is: there is no
primitive root.

WTF? How did this go wrong?

## Assumption: only one chain of length `p**k`

When proving the primitive root theorem, I needed that a chain of
length `p**k` was unique. This was true because a polynomial of degree
`p**k` can have at most `p**k` roots when we are working with fields.

This is *not* true. The factor theorem says that we could factorize
like so:

    (x - a) (x - b) (x - c)

And that was supposed to mean that there are at most three roots
here. Because anything not equal to `a, b, c` would multiply three
non-zero numbers.

But when we are working `mod n` we *can* have `xyz = 0` when none are
zero. So this shows that factorization doesn't mean that there are
only `k` roots.

This is good. It shows that my primitive root theorem proof wasn't
wrong. If the proof "worked" here, it would be proving that the
multiplicative group of `Z mod n` is cyclic, which is not always true
(because Carmichael).

## Maximal chains

I think I can still say this:

1. Crossing two coprime chains yields a chain with `p_1**k_1 p_2 k**2`
   elements and contains the original chains.
2. So there *is* a maximal size if you pick one copy of each of the
   chains of length `p_i**k_i`.
3. There may be many different versions of this maximum chain
   length. But if there are `x` versions of the maximum chain length
   `c`, then we still must have `xc = phi(n)`.

NOTE: I think this relies on:

    No two chains can intersect unless one is contained as a subgroup
    of the other.

    AND

    If there is a chain of length p**k, then every chain of length
    p**(k-1) should be contained in a chain of length p**k.

I haven't proven these, but I think that for now, I am content with my
understanding of the situation.
