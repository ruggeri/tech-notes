## Note: not every element is invertible mod `n`

Consider the chain: `x, x^2, x^3, ...`. By process of elimination, we
know that the chain must eventually cycle back to `x`, or go to zero.

Every element must eventually cycle back to itself or to zero, if
continuously exponentiated. If `n=pq`, then `x**i` can never be
divisible by `n`. But if `n=p**2` then `x**i` can end up at zero.

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

Let's consider my approach to proving Fermat's little theorem:

1. We can take all chains of the form `a**i`.
2. Any such chain can be broken down until we have a chain of length
   `p**k`.
3. Previously, I then showed that only one chain of length `p**k` can
   exist (because a polynomial of degree `p**k` can only have `p**k`
   roots).
    * (This isn't true in the case of `Z mod n`; more in a second).
4. Multiplying two chains of coprime order gives a new chain with
   a new order equal to the product of the old orders.
    * (This is still true in `Z mod n`).
4. This then implies the primitive root theorem.
    * (Not true in `Z mod n` because 3 is not true).
5. Which implies Fermat's little theorem.

## More than `k` `k`th roots of unity is possible.

This will not work with `mod n`. The primary reason is that the a
polynomial of degree `k` can have more than `k` roots when working `Z
mod n`. The factor theorem says that we could factorize like so:

    (x - a) (x - b) (x - c)

And that was supposed to mean that there are at most three roots
here. Because anything not equal to `a, b, c` would multiply three
non-zero numbers.

But when we are working `mod n` we *can* have `xyz = 0` when none are
zero. So this shows that factorization doesn't mean that there are
only `k` roots.

## Chains can intersect

An implication of a single chain of length `p**k` meant that any chain
of length `p**i` (`i < k`) must be contained in that chain.

This is *not* true when working `Z mod n`. When working mod 35, then
you have three order two elements: `6, 29, 34`. But only 29 has a
square root. That means that while `29` is in a length `2**2 = 4`
chain, `6, 34` are not.

This is clearly implied by the fact that you can have a chain of
length `p` not contained in a chain of length `p**2`. Consider if `a`
is in an order four chain:

    (x, x**2 = a, x**3, x**4 = 1)

While `(b, 1)` is contained in no order four chain. Then:

    (xb, x**2 b**2 = a 1, x**3 b, x**4 b**4 = 1 1 = 1)

is a new chain. It intersects the old one, but creates new elements.

## Counting argument...

Note that, overall, even though there are not two distinct chains of
length `p**k`, the duplicate value `a` in the new chain is offset by
the length two chain which contains `b`.

I believe this shows that, effectively, even if you multiply maximal
chains, the double counting you get is equal to the number of unique
elements found in smaller chains.

This makes it "effectively" as if there were possible `n_i` chains of
length `p_i**k_i`, each *non-intersecting*.

Note that, if this were true, then even though there are multiple
choices of chain for each of the `p_i**k_i`, then still we have that
the total number of elements in the group is:

    \prod n_i p_i**k_i

Which would mean that the `p_i**k_i` must divide `phi(n)`.









## Note: not every element is invertible mod `n`

Consider the chain: `x, x^2, x^3, ...`. By process of elimination, we
know that the chain must eventually cycle back to `x`, or go to zero.

Every element must eventually cycle back to itself or to zero, if
continuously exponentiated. If `n=pq`, then `x**i` can never be
divisible by `n`. But if `n=p**2` then `x**i` can end up at zero.

Regardless, even if `x**i` cycles back to `x`, this does *not* mean
`x` is invertable. Indeed, consider any `x` dividing `n`. Then we
have:

    xk = 0

So if we were able to invert `x`, we could multiply by `x**-1` to get:

    k = 0

Note that no element `x` not coprime with `n` is invertable. Since the
"missing" factors of `n` from `x` are `<n`.
