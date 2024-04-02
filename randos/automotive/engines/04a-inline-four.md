# Inline Four

## Flatplane Crank Interval

```
# Flatplane crank
# Pin 1 is at 0deg, Pin 2 is at 180deg, Pin 3 is at 180deg, and Pin 4 is at 0deg.

-   0deg - 180deg: P1 combustion,  P2 compression, P3 exhaust,     P4 intake
- 180deg - 360deg: P1 exhaust,     P2 combustion,  P3 intake,      P4 compression
- 360deg - 540deg: P1 intake,      P2 exhaust,     P3 compression, P4 combustion
- 540deg - 720deg: P1 compression, P2 intake,      P3 combustion,  P4 exhaust

# Note: Power is constantly being delivered, with no silence between
# power strokes.
# Note: Primary reciprocating balance. Also primary rotational balance.
# Note: Secondary reciprocating imbalance. Secondary rotational balance.
```

## Flatplane Crank Balance Analysis

- The flatplane crank engine is like running two nested 360deg twins.
  The outer cylinders are a 360deg twin, and the inner cylinders are a
  second 360deg twin with opposite orientation (rotated by 180deg).
- Thus, we expect the flatplane inline four to inherit the primary
  rotational balance of the 360deg twin.
- We also expect the flatplane inline four to have primary reciprocating
  balance, because the two 360deg twin copies are out of phase by
  180deg.
- Primary reciprocating balance: We sum four primary force waveforms.
  Two are offset by 0deg and two are offset by 180deg. The second pair
  are the negation of the first pair. Thus they cancel.
  - This is what we expect, because we've nested two 360deg twins and
    they each start with reciprocating balance.
- Primary rotational balance: We sum four primary force waveforms, but
  we need to do a few things. First, we need to scale all the waveforms
  based on their distance from a central point. The outer two cylinders
  are at 3x the distance of the inner two cylinders. Second, we need to
  invert the waveforms of two waveforms on one of the sides.
  - So consider the outer pistons. They are equally weighted. They are
    0deg out of phase, but one gets inverted. So they cancel each other.
  - The same thing happens for the inner pistons. They are equally
    weighted. One of them is inverted, so it cancels.
  - Please note that a different setup like 0deg, 0deg, 180deg, 180deg
    (two nested 180deg twins) would have very bad primary rotational
    imbalance (summed).
  - Note that a setup of 0deg, 180deg, 0deg, 180deg (two oppositely
    oriented nested 180deg twins) would have mediocre rotational
    imbalance. That's because the outer and inner pairs would generate
    torques that are opposed but not equal.
- Secondary reciprocating imbalance: here we want to sum the four
  secondary force waveforms. One pair is supposed to be out-out-of-phase
  by 180deg from the second pair. Except that secondary force waveforms
  run at twice the frequency, so they are actually 360deg out-of-phase.
  That is, all four waveforms sum perfectly constructively. The
  secondary reciprocating imbalance is four times the secondary
  imbalance of a single cylinder.
- Secondary rotational balance: again, we want to sum four secondary
  waveforms. As said before, they are all identical and perfectly
  in-phase. As before, they all need to get scaled by distance from
  central point, and the two waveforms of one side of the engine need to
  be negated. That means the outer pair negate each other, and the inner
  pair negate each other.
  - This is what we expect, because we've nested two 360deg twins and
    they start with secondary rotational balance.

## Flatplane Inline Four Discussion

- Generally used for high revving, high horsepower machines.
- Kawasaki, Honda, Suzuki, and Yamaha all make inline four liter bikes.
  They also all make 650cc inline fours, too. Well, I think Yamaha
  just recently discontinued the YZF-R6.
- BMW also makes 1000cc inline fours. They don't do a 650cc inline four.
- Other Europeans (Aprilia, Ducati, KTM) don't do inline fours. They do
  V4s. Triumph builds triples.
- Fours generally are built when you want at least 100HP. Otherwise, you
  can more easily achieve it with a twin.
- To get this, presumably you're normally relying on the high revving of
  the engine to achieve greater HP. Thus, you'll tune it to produce peak
  torque high up in the rev range.
- Even firing of 180deg with no silence results in smooth power
  delivery.
- Firing order is typically 1-3-4-2. I think you could also do 1-2-4-3.
- Other crank configurations like 0deg, 180deg, 0deg, 180deg would lose
  primary rotational balance, but not improve secondary reciprocating
  imbalance.
- To reduce the secondary reciprocating imbalance, I believe a balance
  shaft is used on higher displacement engines.
- There is always one cylinder firing, but there is some pulsation in
  power delivery. That happens because the end of the power stroke
  delivers less power than the beginning.
  - But this is inevitable in all engines with no more than 4 cylinders,
    because a single power stroke is only 180deg, and you only have four
    power strokes per 720deg of crank rotation.
- Because there is always a cylinder firing, the exhaust note is very
  even. This gives a screaming sound at high RPM.

## Crossplane Crank Timing

```
# Crossplane Crank
# Pin 1 is at 0deg, pin 2 is at 90deg, pin 3 is at 270deg, and pin 4 is
# at 180deg.
# This is called "crossplane" because the crankpins like in two
# perpendicular planes that intersect at the crank axis.

-   0deg -  90deg: P1 combustion,  P2 intake,      P3 exhaust,     P4 compression | (1x power)
-  90deg - 180deg: P1 combustion,  P2 compression, P3 intake,      P4 compression | (1x power)
- 180deg - 270deg: P1 exhaust,     P2 compression, P3 intake,      P4 combustion  | (1x power)
- 270deg - 360deg: P1 exhaust,     P2 combustion,  P3 compression, P4 combustion  | (2x power)
- 360deg - 450deg: P1 intake,      P2 combustion,  P3 compression, P4 compression | (1x power)
- 450deg - 540deg: P1 intake,      P2 exhaust,     P3 combustion,  P4 compression | (1x power)
- 540deg - 630deg: P1 compression, P2 exhaust,     P3 combustion,  P4 combustion  | (1x power)
- 630deg - 720deg: P1 compression, P2 intake,      P3 exhaust,     P4 combustion  | (silence)

# We see a firing intervals of 180deg, 90deg, 180deg, 270deg.
# Firing order is 1, 4, 2, 3. (Or 1, 3, 2, 4, if you use the opposite
# orientation).

# Source: https://www.yamahapart.com/crossplanecrankshaft
# Clearly indicates there is only 90deg of silence, and 90deg of overlap.
```

## Crossplane Crank Frequency Analysis

- Primary reciprocating balance: sum four identical waveforms, each
  offset by 90deg. But the 0deg and 180deg waveforms and the 90deg and
  270deg waveforms negate. Thus we have perfect primary reciprocating
  balance.
- Primary rotational imbalance: We need to negate two of the primary
  waveforms before summing them. This is equivalent to a 180deg
  rotation. Thus, cylinder 1 and and cylinder 4's waveforms
  constructively interfere, as do cylinder 2 and cylinder 3's. This is
  worst-case primary rotational imbalance.
  - Note that the outer cylinders contribute more as a rocking couple,
    because they are more distant from the center-of-mass.
- Secondary reciprocating balance: the secondary force waveforms run at
  double the frequency. Thus, instead of four waveforms offset by 90deg,
  they are each offset by 180deg. Thus cylinder 1 and cylinder 2 cancel
  each other, as do cylinders 3 and 4.
- Secondary rotational imbalance. We negate the waveforms for cylinders
  3 and 4, so that we can calculate a torque. We also need to weight by
  distance from the center of mass.
  - Cylinders 1 and 4 have the same secondary waveform, so when cylinder
    4's waveform is negated, it cancels the waveform of cylinder 1.
  - Cylinders 2 and 3 also have the same secondary waveform before
    cylinder 3's waveform is negated. They should cancel, too.

## Crossplane Crank Discussion

- Yamaha is the only one currently doing this for inline fours.
- The YZF-R1 is the only bike that uses this. It's a 150HP liter bike.
- There is 90deg of silence, and 90deg of overlap.
- D4A suggests that you can use either a crankshaft counterweight or a
  balance shaft.
- D4A seems to suggest that that crossplane I4 still has some secondary
  imbalance?
  - I think he might be wrong?
  - Cycle World seems to suggest he is:
    https://www.cycleworld.com/sport-rider/crossplane-what/
  - They explicitly reference primary and secondary balance, and
    explicitly call out a "primary rocking couple." They don't mention a
    secondary rocking couple.
  - So I think they agree with me.
- D4A describes why this bike exists. It's to give the tire recovery
  time to find traction again on corner exit.
  - He suggests that the R6 doesn't use crossplane because the 600 class
    is not overpowered for the exit.
- Basically, this means the bike will have more vibration, and this
  really only benefits you when pushing to the absolute limit in
  cornering.
- Last, I believe this is why you only see this in the Yamaha R1,
  which makes like 160+ HP and is meant for racing.
- Source: https://www.youtube.com/watch?v=uM-ycHS9uvw

## Examples/Uses

- You have a lot of 1000cc inline 4 liter bikes. Especially amongst
  Japanese bikes, but also BMW.
  - Suzuki GSX-R1000, Honda CBR1000RR, Kawasaki Ninja ZX-10R, BMW
    S1000R, Yamaha MT-10, Yamaha YZF-R1.
  - Most of these are set up as nakeds. A number are also set up with
    fairings as race bikes.
  - But they typically have a sport tourer version: Suzuki GSX-S1000GT,
    BMW S1000XR.
  - These all make about 150-200HP.
- There's also typically a huge tourer version: the Kawasaki Concours14
  (canceled?), Yamaha FJR1300ES, for instance. Those are huge and heavy.
  - But those touring motorcycles start to compete with six cylinder
    motorcycles like the Goldwing, which can give more power and run
    smoother.
- Then you have a bunch of supersport bikes that are around 650cc.
  - They tend to make about 100HP.
  - Examples include Honda CBR650R, Kawasaki ZX-6R, Suzuki GSX-R600,
    Yamaha YZF-R6.
  - These bikes are agile and smaller. But they would be wildly
    impractical for anything other than racing (a passenger, luggage,
    highway comfort).
