## Lagrange's Theorem

Statement: In *any* group `G` (not just `Z mod p`), the *order* of every
subgroup `A` (i.e., the number of elements in `A`) divides the *order*
of the group `G` (i.e., the number of elements in `G`.

## Proof of Lagrange's Theorem

Consider the subgroup `A`. Then consider every *coset* `bA`. A coset is
*not* a subgroup. It's the set of elements `ba` where `a \in A`.

**All cosets have the same size**

I say that the size of `bA` is equal to `A`. Why?

Since `G` is a group, the inverse `b^{-1}` exists. Therefore, we know
that:

    b**-1 bA = A

If two `b a_0, b a_1` mapped to the same value, they could not invert
back to separate values.

**Cosets are either disjoint or identical**

Next, let us consider two distinct `b_0 A` and `b_1 A`. I say that they
must either be identical, or completely disjoint. If they are not
disjoint, then there are `a_0, a_1` such that:

    b_0 a_0 = b_1 a_1
    =>
    b_1 = b_0 a_0 a_1^{-1}

Note that `a_0 a_1^{-1}` is guaranteed to be in the subgroup `A`
(subgroups have inverses, are closed).

Now, for any `b_1 a` in `b_1 A`, we have:

    b_1 a = (b_0 a_0 a_1^{-1}) a = b_0 (a_0 a_1^{-1} a)

Thus every element of `b_1 A` is also in `b_0 A`. And vice-versa.
Therefore, if `b_0 A, b_1 A` have any elements in common, they are
identical.

**Cosets partition G**

The union of the set of cosets `b A` is `G` (since `1 \in A`, and `b`
can be any element of `G`). Each coset has the same size, and they are
non-overlapping. Therefore, the distinct cosets *partition* the group
`G`.

Thus the order of the subgroup `A` divides the order of the group `G`.
