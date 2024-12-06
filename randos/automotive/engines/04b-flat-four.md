# Flat (Boxer) Four

## Timing

```
# Consists of two pairs of cylinders. Each pair is a boxer. Cylinders 1
# and 3 are on one side of the crank, and cylinders 2 and 4 oppose them
# on the other side. The crank throws are 0deg, 180deg, 180deg, 0deg.

-   0deg-180deg: P1 combustion,  P2 intake,      P3 exhaust,     P4 compression | power
- 180deg-360deg: P1 exhaust,     P2 compression, P3 intake,      P4 combustion  | power
- 360deg-540deg: P1 intake,      P2 combustion,  P3 compression, P4 exhaust     | power
- 540deg-720deg: P1 compression, P2 exhaust,     P3 combustion,  P4 intake      | power

# Even firing interval of 180deg.

# Firing order is 1-4-2-3. But I think 1-3-2-4 is just as natural
# (reversed order).
```

## Balance and Frequency Analysis

- Primary translational balance: cylinder 1 and cylinder 2 are "in
  phase" except for having opposite orientation. So their primary forces
  will cancel each other. The primary force waveforms for cylinder 3 and
  4 will also cancel each other. Thus we have no net primary force on
  the engine.
  - Each individual boxer already has primary translational balance.
  - So we absolutely expect a composition of two boxers to have it, too.
- Primary rotational balance. Cylinders 1 and 4 are 180deg out-of-phase
  because of the crank angle. When one is at TDC, the other is at BDC.
  However, they are oriented in the opposite direction. Thus, we should
  invert one of the primary force waveforms. That is: the primary force
  waveforms for cylinders 1 and 4 are exactly equal. Last, because we
  want to calculate a _torque_, we should invert one of these waveforms
  again. That brings them out-of-phase, and they cancel.
  - The same reasoning applies to cylinders 2 and 3.
  - This makes sense. An individual boxer twin has a primary rotational
    imbalance which cycles at the same rate as the crank.
  - Our second boxer is identical but 180deg out-of-phase with the first
    one because of the crank throws. Thus, the torque it is generating
    is 180deg out-of-phase with the torque of the first boxer.
  - The two torques from the individual boxer twins should cancel.
- Secondary translational balance: the secondary force waveforms for
  cylinders 1 and 2 are "in phase" except that they have opposite
  orientation. Cylinders 1 and 2 are both at TDC (or at BDC) at the same
  time. Because of the opposite orientation, their secondary force
  waveforms cancel just as before. Same for cylinders 3 and 4. Thus, we
  have secondary translational balance.
  - Each individual boxer already has secondary translational balance.
  - So we absolutely expect a composition of two boxers to have it, too.
- Secondary rotational imbalance.
  - All four cylinders have the same secondary force waveform, before
    considering orientation.
  - That's because all cylinder positions are either 0deg or 180deg
    apart, and secondary force waveform runs a 2x the frequency.
  - Take cylinder 1 and 4; the secondary waveform is "in phase", except
    that they are oriented oppositely. Thus they are really "out of
    phase". That generates a torque.
  - On the other hand, cylinders 2 and 3 generate a torque in the
    opposite direction, because their orientation is reversed.
  - So these two torques mostly cancel. The outer torques are a bit
    stronger because they occur at 1.5x crank pin lengths from the CoM,
    versus the inner torque acts at 0.5x crank pin lengths from the CoM.
  - That gives a total torque which is the secondary waveform multiplied
    by a distance of 2x crank pins.
  - This is exactly the same as summing the torques generated by the two
    individual boxer twins. Each boxer twin generates a torque
    equivalent to a secondary waveform acting at a distance of 1x crank
    pin.
    - So two boxer twin copies generates twice the secondary rotational
      imbalance. As expected.

## Discussion

- This is also called a Boxer 4.
- It was used on 1974-1987 Honda Gold Wing.
  - Which then eventually upgraded to a flat six.
- Today, this engine is mostly used by Subaru.
  - In the Subaru, I think they like that the weight is low.
  - I think also they prefer the flat four to the inline four maybe
    because it gives space for the AWD?
- I am not aware of an 180deg v-angle V4 engines. I think they are just
  inferior to the boxer four.
- Compared to the boxer twin, this has no gaps in power, and gains
  primary rotational balance.
- Compared to the inline four, the boxer four gains secondary
  reciprocating balance but loses secondary rotational balance.
  - However, because of the small offset of crank pins, maybe secondary
    imbalance is not very great. So this cold be better than the I4.
  - Both have even firing interval of 180deg.
- Source: https://youtu.be/TQlyS2rw-sk?t=1220
  - D4A compares applications of inline four, V4 and boxer 4.