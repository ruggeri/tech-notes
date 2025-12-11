# Flat (Boxer) Six

## Timing

```
# I believe this is three boxer twins, each successive one being 120deg
# offset from the prior one. You can see this also as a composition of
# two opposed 120deg inline threes.
#
# Let's label the cylinders R1-R3 and L1-L3.

-   0deg-180deg: R1 combustion
- 120deg-300deg: L3 combustion
- 240deg-420deg: R2 combustion
- 360deg-540deg: L1 combustion
- 480deg-660deg: R3 combustion
- 600deg-780deg: L2 combustion (through 60deg)

# I believe this is the firing order of Porsche flat sixes, for
# instance.
#
# This gives an even firing interval of 120deg, just like the inline
# six.
```

## Balance

- Primary translational balance: each individual boxer pair has primary
  translational balance, so we expect the composition to have it.
  - Likewise, the individual triple banks have primary translational
    balance, so the two banks should have it.
  - But let's check. The three cylinders in a bank are all 120deg out of
    phase. As three roots-of-unity, they sum to zero.
  - This applies for both sides (which just have a different
    orientation).
- Primary rotational balance.
  - This is a bit tricky. Let `w` be the length of a crank pin.
  - Then L2 has center at `+w/2` and R2 has center at `-w/2`.
  - L1 has center at `+5/2 w` and R1 has center at `+3/2 w`.
  - L3 has center at `-3/2 w` and R3 has center at `-5/2 w`.
  - This makes sense: each successive cylinder is distance `w` apart.
  - Note that L1 is farther from the CoM than L3. We cannot trivially
    combine their torques.
  - Let's start calculating torques.
    - L1: `+5/2 w * cos(theta)`.
    - R1: `-3/2 w * cos(theta)`. (same side, different distance, different orientation)
    - L2: `+1/2 w * cos(theta + 120)`.
    - R2: `+1/2 w * cos(theta + 120)`. (same distance, different side, different orientation).
    - L3: `-3/2 w * cos(theta + 240)`.
    - R3: `+5/2 w * cos(theta + 240)`.
  - We can now sum.
    - L1+R1: `w * cos(theta)`.
    - L2+R2: `w * cos(theta + 120)`.
    - L3+R3: `w * cos(theta + 240)`.
  - And of course this sums to zero torque.
  - This was a little tough, since there is no pairwise cancellation of
    torques. I made several mistakes
- Secondary translational balance: each boxer pair has secondary
  translational balance. Each triple bank has secondary translational
  balance. This has secondary translational balance.
  - We can double-check. A bank consists of three cylinders at crank
    angles 120deg out-of-phase. But secondary vibration runs at 2x the
    frequency. But that just means that they are 0deg, 240deg, and
    120deg out-of-phase.
  - Thus, again, the sum of the secondary forces is zero.
- Secondary rotational imbalance. Just barely imbalanced though.
  - The same argument as before applies. The only difference is that
    instead of having torques 0deg, 120deg, and 240deg out-of-phase, we
    have secondary torques that are 0deg, 240deg, and 120deg
    out-of-phase.
  - Thus they cancel as before.

## Discussion

- This is most associated with Porsche.
  - Subaru also used to make one, I think.
- On motorcycles, most identified with the current Honda Gold Wing.
- I don't believe any 180deg V6 engines are made. So boxer sixes are the
  only flat sixes.
- Compared to inline six, these are much shorter. They can get weight
  lower on the car.
- D4A gives a number of reasons why the flat/boxer six engine might have
  a lighter crankshaft.
- Main downside versus inline six are that it is wide, so hard to fit in
  an engine bay, and it has two cylinder heads/camshafts/manifolds.
- Compared to an H4, gains secondary rotational balance.
  - And 60deg of power stroke overlap.
  - And of course it can make more power, by hitting higher RPM because
    each cylinder is smaller and each piston is lighter.
- An H6 is shorter than an I6, so maybe easier to package in a car.
  - Except the H6 is wide. Which is maybe why it's only used in wide
    sports cars?
  - And H6 gives a lower CoM, which improves handling.
  - Which is why I think Porsche uses them in sporty vehicles.
- Source: https://youtu.be/mTS48jX68YU?t=733
