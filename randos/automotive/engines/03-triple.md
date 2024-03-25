# Triple

## 120deg Crank Timing

```
# 120deg crank. Three crankpins, each offset by 120deg.
# Pin 1 is at 0deg, pin 2 at 120deg, pin 3 at 240deg, in the direction of crank rotation.

-   0deg -  60deg: P1 combustion,  P3 intake,      P2 exhaust     (power)
-  60deg - 120deg: P1 combustion,  P3 compression, P2 exhaust     (power)
- 120deg - 180deg: P1 combustion,  P3 compression, P2 intake      (power)
- 180deg - 240deg: P1 exhaust,     P3 compression, P2 intake      (silence)
- 240deg - 300deg: P1 exhaust,     P3 combustion,  P2 intake      (power)
- 300deg - 360deg: P1 exhaust,     P3 combustion,  P2 compression (power)
- 360deg - 420deg: P1 intake,      P3 combustion,  P2 compression (power)
- 420deg - 480deg: P1 intake,      P3 exhaust,     P2 compression (silence)
- 480deg - 540deg: P1 intake,      P3 exhaust,     P2 combustion  (power)
- 540deg - 600deg: P1 compression, P3 exhaust,     P2 combustion  (power)
- 600deg - 660deg: P1 compression, P3 intake,      P2 combustion  (power)
- 660deg - 720deg: P1 compression, P2 intake,      P2 exhaust     (silence)

# We see that 180deg of power is followed by 60deg of silence.
# Power is delivered evenly.
```

## 120deg Crank Balance Analysis

- Primary reciprocating balance.
  - This is the sum of the three individual primary force waveforms.
    They are identical, except each is offset by 120deg.
  - I have proven elsewhere that the sum of evenly offset waveforms is
    always zero. This is the sum of three third-degree roots of unity.
  - Thus, we have perfect primary reciprocating balance.
- Primary rotational imbalance.
  - We will focus on rotational imbalance about the center-of-mass of
    the engine, since that is what matters in terms of transmission of
    vibration to frame.
  - The center of mass lies approximately at the center piston.
  - Thus, the center piston contributes nothing to the rotational
    imbalance.
  - We must sum the primary forces of the two outside pistons, after
    inverting one of them (because this is a rotational calculation).
  - The two pistons start out at 240deg out-of-phase. That's 120deg
    out-of-phase if just looked at with a reversed perspective. And thus
    we see that inversion doesn't change anything.
  - What is the sum of two waveforms that are 120deg out-of-phase? We
    know it's the third waveform inverted.
  - I believe that a triple should have rotational imbalance somewhere
    between a 180deg twin of 66%-100% of the triple's displacement.
    - Consider 2/3rds displacement 180deg twin.
    - 180deg twin has more constructive interference of primary torques
      (2x vs 1x).
    - Cylinder size/shape is the same (assumes same bore-to-stroke). But
      the cylinders are closer together (0.5x vs 1x).
    - But also if compression is same, force in cylinders from
      combustion should be less?
    - Not entirely sure of this analysis.
- Secondary reciprocating balance.
  - We need to sum the three individual secondary force waveforms. These
    are again offset by 120deg, but the secondary force waveform runs at
    twice the frequency.
  - Thus, they are each offset by 240deg. But wait, that means they're
    still offset by 120deg each, just in the opposite direction as
    before.
  - Thus, the summation is exactly the same as before, resulting in
    perfect secondary reciprocating balance.
- Secondary rotational imbalance
  - Again, we ignore the central cylinder. We sum the other two
    cylinder secondary force waveforms, negating one.
  - Again, those waveforms start out 120deg out of phase. One is
    negated, but that just changes the waveforms to be 240deg out of
    phase.
  - So their sum is again a sinusoidal of the same amplitude, just as
    with the primary rotational imbalance.
- D4A: https://www.youtube.com/watch?v=82rxavW0A3c
  - Describes inline three balance. Verifies primary reciprocating
    balance is good, but primary rotational balance is imperfect.
  - Claims perfect secondary balance; I think he means just secondary
    reciprocating balance.

## 120deg Crank Discussion

- Typically mounted transverse, and the headers will look like a
  "trident" a bit.
  - Exception is Rocket III which has a longitudinal mounted engine.
- Yamaha and Triumph are the only manufacturers that make triples.
- Typically make about 80-120HP.
- The "typical" setup is 120deg crank offset.
  - Yamaha calls this "CP3" (Crossplane 3). But this is the typical and
    traditional 3 cylinder setup.
  - All Yamaha triples use this setup.
  - The branding matches CP2 (their 270deg twin) and CP4 (a crossplane 4
    engine) that Yamaha sells.
- As noted above, the firing interval is an even 240deg.
- Firing order is either 1-3-2 or 1-2-3, depending on the orientation of
  your cylinder numbering.
- Power delivery is not perfectly even, because there are moments when
  there are no power strokes occurring (180deg to 240deg, 420deg to
  480deg).
  - However, this is more even power delivery than any twin engine.
  - There is an even 60deg of silence between combustion cycles.
- Perfect primary and secondary reciprocating balance is achieved, as
  noted above.
- A 120deg triple will have some primary rotational imbalance.

## T-Plane Timing

```
# Three crankpins, each offset by 90deg. Pin 1 is at 0deg, pin 2 is at
# 90deg, pin 3 is at 180deg.

-   0deg- 90deg: P1 combustion,  P2 intake,      P3 exhaust     | (power)
-  90deg-180deg: P1 combustion,  P2 compression, P3 exhaust     | (power)
- 180deg-270deg: P1 exhaust,     P2 compression, P3 intake      | (silence)
- 270deg-360deg: P1 exhaust,     P2 combustion,  P3 intake      | (power)
- 360deg-450deg: P1 intake,      P2 combustion,  P3 compression | (power)
- 450deg-540deg: P1 intake,      P2 exhaust,     P3 compression | (silence)
- 540deg-630deg: P1 compression, P2 exhaust,     P3 combustion  | (power)
- 630deg-720deg: P1 compression  P2 intake,      P3 combustion  | (power)

# Firing interval is 180deg of combustion, 90deg silence, 180deg
# combustion, 90deg silence, 180deg of combustion. Also notated as
# 270deg, 270deg, 180deg.
```

## T-Plane Frequency Analysis

- Primary translational imbalance: we need to sum three primary force
  waveforms each 90deg out-of-phase. The first and third cancel, but the
  second waveform remains. Primary translational imbalance is thus
  equivalent to a single-cylinder of 1/3rd the displacement.
  - This is worse than the 120deg triple, which has no primary
    translational imbalance.
- Primary rotational imbalance: we need to sum the three primary force
  waveforms, weighted by distance from the CoM. This allows us to ignore
  the middle cylinder. We sum the outer cylinder waveforms, remembering
  to invert one first. Since the outer cylinders start out 180deg
  out-of-phase, inversion brings them into phase. Thus the waveforms
  perfectly constructively interfere.
  - This is 2x as bad as the conventional inline triple.
- Secondary translational imbalance: secondary force waveforms are each
  set 180deg apart. When summing them, one is left.
  - This is inferior to the conventional triple crank configuration,
    which has secondary translational balance.
- Secondary rotational balance: We ignore the central cylinder and focus
  on the outside cylinders. The outside cylinder secondary force
  waveforms are in-phase, but because this is rotational imbalance, we
  need to invert one. The outside cylinders are also at the same
  distance from the CoM. Thus the torques cancel.
  - This is superior to the conventional triple, which has an imbalance.

## T-Plane Discussion

- Triumph's Trident 660, Speed Triple, and Street Triple have crank
  angles of 120deg. We've already discussed that setup.
- Tiger 850, 900, 1200 have "T-Plane."
- The engine loses primary and secondary translational balance.
- It also doubles the primary rotational imbalance, while eliminating
  the secondary rotational imbalance.
  - A bad deal, because secondary rotational balance is much smaller.
- Triumph says the goal was to give more recovery time (during the 90deg
  silence, 1/8th more than in 120deg triple). That could help mostly on
  the dirt, where the tire may lose traction under power, but gets time
  to find traction during the unpowered interval.
- That would explain why only the Triumph ADV bikes have this setup. The
  street ones use a conventional 120deg triple.
- Another factor is to obtain a different sound. This can help your
  motorcycle stand out from the crowd, or offering something unique that
  people might want to pay for.
- The downside is less balance greater vibration.
  - Which is why I think they don't use it on street bikes, where
    there is less benefit.
  - Higher vibration is presumably going to limit overall power.
- Last thought: Kevin Cameron suggests you need a balance shaft anyway
  for a conventional triple (to eliminate the primary rotational
  imbalance). In that case, maybe the T-Plane just requires a
  reconfiguration of balance shaft weight, which can correct the
  imbalances it introduces.
  - I don't presently know enough about balance shafts to explore this
    myself.
- D4A T-Plane Discussion: https://www.youtube.com/watch?v=vU7faKiQleM
- Source Kevin Cameron: https://www.cycleworld.com/story/bikes/triumphs-new-t-plane-firing-order-explained/
