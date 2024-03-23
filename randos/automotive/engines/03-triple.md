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
  - The outside cylinders are placed about twice as far away as they
    would be in an inline twin of 2/3s the displacement.
  - Thus, the primary rotational imbalance should be about equivalent to
    a 180deg twin of 2/3s the displacement.
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
- Compared to 180deg inline twin of 2/3 displacement, I think this has
  (1) equivalent primary reciprocating balance, (2) equivalent primary
  rotational _imbalance_, (3) superior secondary reciprocating balance
  (in fact, perfect), (4) inferior secondary rotational imbalance.
  - You might be able to argue that the inferiority in point (4) is
    equal to the superiority in point (3).
  - In that case, the triple is equivalently balanced to an 180deg
    inline twin of 2/3rds the displacement.
  - In which case, you're getting a third cylinder (and +50%
    displacement) for free.

## T-Plane Timing

```
# TODO: T-Plane timing.
```

## T-Plane Frequency Analysis

**TODO**

## T-Plane Discussion

**TODO**: Review this!

- Triumph's Trident 660, Speed Triple, and Street Triple have crank
  angles of 120deg. We've already discussed that setup.
- Tiger 850, 900, 1200 have "T-Plane."
- T-Plane has crank angles of 0deg, 270deg, and 180deg.
- Cylinder 1 fires 0-180deg. Then cylinder 3 fires 180-360deg. Then
  360-450deg is 90deg silence. Then cylinder 2 fires 450-630deg. Then
  630-720deg is 90deg of silence.
- That gives firing intervals of 180-270-270.
  - Or: 0deg silence, 90deg silence, 90deg silence.
- The engine loses primary and secondary balance.
  - There is not primary reciprocating balance. Two pistons are 180deg
    apart, but there is the third piston which is not balanced. So
    that piston will not be balanced.
  - For primary rotational balance, the two outer pistons form a
    rocking couple.
  - But, usefully, the primary rotational imbalance is about the
    central piston, which is helpful.
- Why do this? And why only on the ADV bikes? The reason they say is
  that even power delivery can mean that the tire is constantly under
  power and breaks traction in dirt and starts to spin.
- By giving a longer interval under no power, the tire can find
  traction, and then you give it a pulse of power.
- It seems implausible, but this was established on earlier big-bang
  firing engines that are used in off-road motorsports.
- The downside is greater vibration.
  - Which is why I think they don't use it on street bikes, where
    there is less benefit.
  - Higher vibration is presumably going to limit overall power.
- Another benefit is that the sound is nice.
- For this reason, twins with 270 crank throws are also popular
  off-road.
- D4A: https://www.youtube.com/watch?v=vU7faKiQleM
