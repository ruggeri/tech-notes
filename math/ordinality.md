## Cardinality

*Cardinality* is a notion of "size" of a set. Two sets have the same
cardinality if they can be put in bijection.

There are sets with finite numbers of elements. The first infinite
cardinality is aleph null. This is the number of integers (and likewise
the number of rational numbers). These are the countably infinite sets.

The cardinality of the reals is 2^aleph_0. The cardinality of the reals
is equal to the cardinality of the power set of rationals?

If a real is defined as the infinum of a set of rationals, it's clear
the cardinality of reals is less than the cardinality of the power set
of rationals.

The continuum hypothesis is that there is no set of cardinality
intermediate between the rationals and the reals. But this is
independent of ZFC.

## Order Type

Two *ordered* sets have the same order type if there is a monotonic
bijection between the two. This is called *order isomorphic*. I'm
assuming a *total order*, though apparently this idea can work even if
you only have a partial ordering.

If two sets are order isomorphic, we say they have the same *order
type*. This is the "ordinality" of the set. Order isomorphism is
transitive/reflexive, so the concept of ordinality partitions ordered
sets into equivalence classes.

We next talk about *well-ordered* sets. These are ones where each subset
has a *least element*.

Being well-ordered means that every element has a unique successor
(except possibly a greatest element). Imagine $x$. What's the minimum of
${ y | x < y }$? That's the successor. It's unique.

Note that the naturals are well-ordered, but not the integers, nor the
positive rationals.

If a set is well-ordered, you can use *transfinite induction*. The axiom
of choice is equivalent to the proposition that every set has a
well-order (not necessarily a natural one). This is called the
*well-ordering theorem*.

The order types of well-ordered sets are the *ordinal numbers*. There
are two types of elements in a well-ordered set: successor and limit
elements.

Ordinal numbers consist of:

* Finite numbers.
* Omega (order type of naturals).
* Omega + 1 (order type of naturals, plus a last greatest number).
* 2omega (two successive copies of naturals).
* omega^2 (length two vectors of naturals).
* omega^omega (set of all finite length vectors of naturals).

Von Neumann defined ordinals recursively. Each ordinal is defined as the
set of all ordinals smaller than it. He did this with null set standing
in as zero.
