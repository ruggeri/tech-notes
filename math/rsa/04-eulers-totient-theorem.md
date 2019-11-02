## Euler's Totient Theorem

Euler's totient theorem says that in `Z mod n`:

    a^{phi(n)} = 1 (mod n)

whenever `a` is *relatively prime* to `n`.

Here `phi(n)` is the number of coprime numbers `<=n`. Note of course
that `phi(p) = p-1`. So this is a generalization of Fermat's little
theorem.

## Proof

We already discussed that the multiplicative subgroup of `Z mod n`
consists exactly of those elements `a` that are relatively prime to `n`.
Of course the size of the multiplicative subgroup is `phi(n)`. That's
the very definition of `phi(n)`.

But then the theorem is trivial. It's the same as Lagrange's theorem.

