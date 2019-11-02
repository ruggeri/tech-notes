## Statement

Fermat's little theorem says that for any `a \ne 0`:

    a^p = a (mod p)

Equivalently,

    a^{p-1} = 1 (mod p)

## Proof

Fermat's little theorem is simply a special case of Lagrange's theorem.

As mentioned in the last section, we already showed that the series
`a^i` generates a subgroup of the *multiplicative subgroup* of `Z mod
p`.

By applying Lagrange's theorem, we showed that the order of the
generated subgroup *divides* `p - 1` (the size of the multiplicative
subgroup).

But then we have that if `|a| k = p - 1`, then:

    a^{p - 1} = (a^{|a|})^k = 1^k = 1

Again, we must note that the order of `a` may be *less* than `p - 1`.
