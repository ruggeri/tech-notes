# Inline Six

## Inline Six Timing

```
# If we nest 3x 360deg inline twins, we'll have primary rotational
# balance (see discussion of 360deg inline twin). If we offset each
# pair of pistons by 120deg, we'll have primary translational balance
# (see discussion of 120deg inline triple).
#
# We expect secondary rotational balance (again, see 360deg inline
# twin discussion). And we expect secondary translational balance
# (again, see 120deg inline triple discussion).
#
# Alternatively, you can view this setup as two inline threes, set in a
# line, but with opposite orientation. The inline three has perfect
# translational balance, but it has primary and secondary rotational
# imbalance. But if we put two opposite oriented ones in a line, we
# expect to retrieve secondary balance.
#
# Thus, crank is 0deg, 120deg, 240deg, 240deg, 120deg, 0deg.

-   0deg- 60deg: P1 comb.,   P2 intake,  P3 exhaust, P4 compr.,  P5 comb.,   P6 intake  (2x power)
-  60deg-120deg: P1 comb.,   P2 compr.,  P3 exhaust, P4 compr.,  P5 exhaust, P6 intake  (1x power)
- 120deg-180deg: P1 comb.,   P2 compr.,  P3 intake,  P4 comb.,   P5 exhaust, P6 intake  (2x power)
- 180deg-240deg: P1 exhaust, P2 compr.,  P3 intake,  P4 comb.,   P5 exhaust, P6 compr.  (1x power)
- 240deg-300deg: P1 exhaust, P2 comb.,   P3 intake,  P4 comb.,   P5 intake,  P6 compr.  (2x power)
- 300deg-360deg: P1 exhaust, P2 comb.,   P3 compr.,  P4 exhaust, P5 intake,  P6 compr.  (1x power)
- 360deg-420deg: P1 intake,  P2 comb.,   P3 compr.,  P4 exhaust, P5 compr.,  P6 comb.   (2x power)
- 420deg-480deg: P1 intake,  P2 exhaust, P3 compr.,  P4 exhaust, P5 compr.,  P6 comb.   (1x power)
- 480deg-540deg: P1 intake,  P2 exhaust, P3 comb.,   P4 intake,  P5 compr.,  P6 comb.   (2x power)
- 540deg-600deg: P1 compr.,  P2 exhaust, P3 comb.,   P4 intake,  P5 compr.,  P6 exhaust (1x power)
- 600deg-660deg: P1 compr.,  P2 intake,  P3 comb.,   P4 intake,  P5 comb.,   P6 exhaust (2x power)
- 660deg-720deg: P1 compr.,  P2 intake,  P3 exhaust, P4 compr.,  P5 comb.,   P6 exhaust (1x power)

# Firing order is 1-4-2-6-3-5. If we ran the crank in the other
# direction, then the crank offsets would be 0deg, 240deg, 120deg,
# 120deg, 240deg, 0deg (relative to direction of crank rotation). In
# that case, firing interval would be 1-5-3-6-2-4, which is the
# "canonical" firing order.
#
# Other firing orders are possible, but I believe these are the
# preferred ones. Something to do with fueling and exhaust manifolds. In
# particular, I believe they want to avoid adjacent cylinders firing in
# succession.

# Note: the crank is constantly under power. There is significant
# overlap of power strokes. The firing interval is an even 120deg.
```

## Inline Six Frequency Analysis

- I won't do a thorough frequency analysis because we've already
  discussed how this is 3x nested 360deg twins. They each have primary
  and secondary reciprocating imbalances each 120deg out-of-phase, thus
  canceling. And each with perfect primary and secondary rotational
  balance.
- Or two opposite orientation 120deg inline triples. Each has perfect
  primary and secondary reciprocating balance. Each has opposite primary
  and secondary torques.

## Inline Six Discussion

- These appear to be most identified with BMW, which continued to focus
  on I6 even as other manufacturers made V6s because they package
  better.
  - I think Mercedes-Benz also made a lot?
- Simple design, because one cylinder head, one set of camshafts, one
  exhaust manifold.
- It has an even firing interval of 120deg. And there is 60deg of power
  stroke overlap.
- The inline six is two mirrored inline threes, which cancels the
  rotational imbalance of a single inline three.
- It's long. I think that makes it hard to achieve larger displacements.

- Source: https://youtu.be/mTS48jX68YU?t=123
  - D4A explains inline 6 and compares it to V6, VR6, and Boxer 6.
- Source: https://youtu.be/82rxavW0A3c?t=832
  - D4A explains Inline 6 and compares it to I3, I4, I5.
