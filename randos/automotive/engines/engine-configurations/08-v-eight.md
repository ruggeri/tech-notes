# V8 Engine

- Is common in American high performance cars.
- Also in Ferraris.
- A common way to scale engine size past an I4, which has some secondary
  balance problems.
- Typically uses a 90deg bank angle. That way you have 90deg of overlap
  between successive power pulses. There are always two cylinders on the
  power stroke.
- A V8 is kind of like two I4s at a 90deg angle.
  - The I4 has a secondary imbalance.
  - Thus if we use the I4 crankshaft, we would expect a side-to-side
    secondary translational imbalance of the engine.
  - The magnitude should be 1.4x.
- The big choice is between a _crossplane_ crankshaft (common in
  American cars), or a _flatplane_ crankshaft (common in European cars).
- We will see that the crossplane cranksaft makes it easier to balance
  the engine.
- On the other hand, the flatplane crankshaft often ends up being
  lighter, though the engine is not as well balanced. That allows a
  higher reving engine.
- I don't believe any flat eights have ever been made for production
  cars. I believe Porsche made some for racecars.

## Crossplane V8 Timing

```
# Bank angle is 90deg.
# Crank is 0deg, 90deg, 270deg, 180deg.
# We typically label cylinders 1-8 based on crankshaft position. The
# even cylinders will all be on the left side, and the odd cylinders
# will be on the right.
# We will assume the crank turns from the left side to the right.

-   0deg- 90deg: 1 (R1) starts combustion.
-  90deg-180deg: 8 (L4) starts combustion.
- 180deg-270deg: 7 (R4) starts combustion.
- 270deg-360deg: 2 (L1) starts combustion.
- 360deg-450deg: 6 (L3) starts combustion. (Forced to fire two L in a row).
- 450deg-540deg: 5 (R3) starts combustion.
- 540deg-630deg: 4 (L2) starts combustion.
- 630deg-720deg: 3 (R2) starts combustion. (But this will be followed by R1.)

# We get even 90deg firing.
#
# Notice that we tried to avoid firing twice on the same side for as
# long as we could. It is inevitable though. This means two exhaust
# pulses on same side, which gives a characteristic rumble (and causes)
# some issues with turbos and also exhaust backpressure.
```

## Crossplane V8 Balance

- Our engine consists of two crossplane I4s. Thus, it inherits primary
  and secondary translational balance.
- Primary rotational balance.
  - L1 and R1 are going to create a primary force that rotates. Multiply
    by the distance from the CoM and we have a torque.
  - L4 and R4 are going to create an equivalent primary force that
    rotates, but 180deg out-of-phase of the L1/R1 force. But we're
    dealing with _torque_, so we have to multiply by the distance in the
    _opposite_ direction.
  - Thus, these two torques should constructively interfere.
  - How do we solve this? We can use a crankshaft counterweight that
    cancels the primary force of L1/R1.
  - A corresponding counterweight (180deg out-of-phase) cancels the
    primary force of L4/R4.
  - The counterweights have no effect on primary translational balance,
    since they are 180deg out-of-phase. And they cause no secondary
    forces.
  - We need to add counterweights for L2/R2 and L3/R3, too.
- Secondary rotational balance.
  - We can't use counterweights on the crank to cancel a secondary
    force.
  - In good news, we don't have to! L1 and R1 again create a secondary
    force that rotates direction. L4 and R4 again create an equivalent
    but out-of-phase force.
  - However, it's no longer 180deg out-of-phase. Because these are
    secondary forces, they are 360deg out-of-phase.
  - Which is perfect. Because to calculate torque, we multiply by
    distance, with a sign change for direction. The rotating torque of
    L1/R1 cancels the rotating torque of L4/R4 exactly.
  - And likewise for L2/R2 and L3/R3.

## Flatplane V8 Timing

**TODO**

## Flatplane V8 Balance

**TODO**
