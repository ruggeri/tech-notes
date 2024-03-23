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

## Flatplane Crank Frequency Analysis

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

## Crossplane Crank

- **TODO**: Crossplane timing.
- **TODO**: Crossplane frequency analysis.

- Yamaha is the only one currently doing this for inline fours.
- The YZF-R1 is the only bike that uses this. It's a 150HP liter bike.
- Crank angle is 90deg. So pistons are at 0deg, 270deg, 450deg, 180deg
  with respect to each other.
  - This is "crossplane" because the cylinders do lie in two planes
    that intersect at the crankshaft.
  - Notice that the firing order is 1-3-2-4. (Again, could have
    arranged 1-2-3-4, or even 1-4-2-3).
  - Cylinder 1 fires 0deg-180deg, 180deg-270deg of silence, cylinder 3
    fires 270deg-450deg, cylinder 2 fires 450deg-630deg, and cylinder
    4 fires 540deg-720deg.
  - Notice there is 90deg of silence, and 90deg of overlap.
- So let's talk balance. You should have primary reciprocating
  balance. But you should have rocking about the center of mass.
- So you have two rocking couples.
  - But the outer couple is placed further away, so it has more of an
    impact.
  - You could add counterweights at ends of crankshaft to reduced (but
    not eliminate) this rocking couple. We talked about how you could
    do that with 180deg parallel twins.
  - Claims that a balance shaft is a better solution here because...
    Why?
- In good news, you have less secondary imbalance than a flatplane I4.
  - That's because there is no net secondary force at 90deg/270deg.
  - So you only have two pistons (the ones at 0deg, 180deg)
    contributing to secondary imbalance.
  - But this is not much of a win, because secondary imbalances are
    already less important than primary imbalance.
- Talks about big bang engines, which produce their torque/power
  during a short phase of the cycle. The bangs are placed closer
  together.
  - This typically increases vibrations and reduces maximum possible
    output.
  - He talks about the advantage. Which is that in extreme high-speed
    cornering, you're going to get recovery time during which the tire
    is not under power. That way you can regain traction.
  - The crossplane isn't as crazy as some I4s with even more absurd
    firing intervals. But it's on that spectrum.
  - The R6 doesn't use crossplane because the 600 class is not
    overpowered for the exit.
- Basically, this means the bike will have more vibration, and this
  really only benefits you when pushing to the absolute limit in
  cornering.
- Last, I believe this is why you only see this in the Yamaha R1,
  which makes like 160+ HP and is meant for racing.
- Source: https://www.youtube.com/watch?v=uM-ycHS9uvw&t=180s

## Examples/Uses

- You have a lot of 1000cc inline 4 liter bikes.
  - Suzuki GSX-R1000, Honda CBR1000RR, Kawasaki Ninja ZX-10R, BMW
    S1000R, Yamaha MT-10, Yamaha YZF-R1.
  - Most of these are set up as nakeds. A number are also set up with
    fairings as race bikes.
  - But they typically have a sport tourer version: Suzuki GSX-S1000GT,
    BMW S1000XR.
  - These all make about 150-200HP.
- There's also typically a huge tourer version: the Kawasaki Concours14
  (canceled?), Yamaha FJR1300ES, for instance. Those are huge and heavy.
- Then you have a bunch of supersport bikes that are around 650cc.
  - They tend to make about 100HP.
  - Examples include Honda CBR650R, Kawasaki ZX-6R, Suzuki GSX-R600,
    Yamaha YZF-R6.
  - These bikes are agile and smaller. But they would be wildly
    impractical for anything other than racing (a passenger, luggage,
    highway comfort).
- Just a few brands make a 1000cc bike which makes ~100HP.
  - Kawasaki doesn't make triples, so it offers the Ninja 1000SX.
