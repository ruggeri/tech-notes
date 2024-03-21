## V-Twin

```
## TODO: Copy timing chart from 270deg Inline Twin .
```

**Mounting**

- Almost always transverse mounted (crankshaft perpendicular to bike
  body; easily drives chain).
  - Exception is Moto Guzzi, which mounts longitudinal (crankshaft
    parallel to bike body).
  - That's an advantage for air cooling, but a disadvantage for width.
  - I believe Moto Guzzi uses a crank shaft.

**Common Examples**

- V-Twins are not that common in bikes.
  - Harley-Davidson bikes.
  - Ducati focuses on V-twins. KTM's biggest adventure bikes. And
    Suzuki's V-Stroms.
  - BMW doesn't make any (they do flat twin), nor does Honda, Kawasaki,
    or Triumph.
  - Probably Japan didn't do them because a V-Twin tends to be more
    expensive (two cylinder heads).
- Ducati V-twins are 90deg and mounted with one cylinder pointing almost
  forward; they thus call it L-Twin.
  - They sometimes call this engine "Testastretta 11°"; the 11 refers to
    something totally unrelated to what we're discussing (valve
    overlap).
  - Like all 90deg V-Twins I know, uses a shared crank pin and fires on
    a 270-450 interval.
    - 180deg of power, 90deg of silence, 180deg of power, 270deg of
      silence.
  - Transverse mounted.
  - No rocking couple because of single crank pin.
    - Well, that isn't true. Even though there's one crank pin, there
      are two side-by-side connecting rods. Which means you have some
      rotational imbalance.
    - But it's a lot less.
  - Ducati doesn't do any parallel twins. V-Twins are used for their
    smaller displacement bikes.
  - A downside to this design is that you need a longer wheelbase to
    accommodate the horizontal cylinder. That impacts maneuverability.
    - Though it's not a big problem for cruiser bikes or adventure
      bikes.
- Moto Guzzi V7 Engine
  - Longitudinal mounted, 90deg V-twin.
  - Uses a single crank pin. 270-450 firing interval.
  - No rocking couple because of single crank pin.
- Suzuki V-Twins (SV650 and V-Strom bikes)
  - 90deg angle. Fires on 270-450 interval like every other 90deg
    V-Twin.
- I think the major downside to 90deg V-Twin is that it has a lot of
  deadspace between the cylinders and it's hard to fit. That's why
  Suzuki might use it on V-Stroms which aren't intended to be that
  maneuverable, or Moto Guzzi might try to mount longitudinally.
  - Note that the Ducati Supersport 950 does have a fairly long
    wheelbase for an alleged sport-bike.
- I think geometry might allow you to get longer stroke than in an
  inline engine. That might help with displacement vs an inline twin,
  for the same bike height.

## Operation

```
# Shared crank pin. But piston movement is out of phase by 90deg.
# We'll see that this is the same sound as the 270deg inline twin, but the forces are arranged differently.
# I'll imagine crankshaft spins from left piston to right. Right will fire first.

-   0deg -  90deg: Piston R combustion A,  Piston L intake B      (power)
-  90deg - 180deg: Piston R combustion B,  Piston L compression A (power)
- 180deg - 270deg: Piston R exhaust A,     Piston L compression B (silence)
- 270deg - 360deg: Piston R exhaust B,     Piston L combustion A  (power)
- 360deg - 450deg: Piston R intake A,      Piston L combustion B  (power)
- 450deg - 540deg: Piston R intake B,      Piston L exhaust A     (silence)
- 540deg - 630deg: Piston R compression A, Piston L exhaust B     (silence)
- 630deg - 720deg: Piston R compression B, Piston L intake A      (silence)

# Note: uneven interval of silence between power pulses. 90deg then 270deg of silence between pulses.
```

- You can use a counterweight that is at 180deg from the crankpin.
  This will always rotate opposite the crankpin. This will be
  positioned exactly opposite to the cylinder whenever it fires.
- Without the counterweight, you will have a rotating imbalance in the
  engine. With the counterweight, you achieve perfect primary balance.
- Technically you do have a rocking couple, but it is minimized because
  the shared crank pin means the cylinders are almost in same plane.
- Also: you will have _secondary_ imbalance, just like with an inline
  twin.
  - **TODO**: What is the magnitude?

## Comparison With Inline Twins

- The uneven firing interval gives a characteristic sound, but it also
  means uneven power delivery.
  - Not worse than the 180deg inline twin though...
  - This could be good on dirt, because it allows the tire to recover
    traction.
- The 270deg parallel twins have a lot of the same characteristics as
  the V-twins.
- I can't find any justification for the belief that V-Twins make more
  low-end torque than inline twins.
  - I think that maybe V-Twins tend to have longer strokes, because of
    their geometry.
  - But then we ask again whether longer strokes even make more torque.
  - And we also have the question: do we care about engine torque if we
    can always adjust gearing to produce more torque at the wheel?
    - I think we do. Broader torque means we spend less time clicking
      through gears.
  - I think that if the engine produces more torque down low, you can
    accelerate better in first gear?
- I think that the main tradeoff is really about packaging and cost.
  - It's easier to fit an inline twin into a bike.
  - Cost to produce and maintenance are cheaper on the inline twin. You
    only have one case, valvetrain, et cetera.
- OTOH, V-Twin might be narrower (when mounted transversely), because
  it's only one cylinder.

## Sources

- Source:
  https://www.cycleworld.com/2015/12/03/ask-kevin-cameron-honda-africa-twin-parallel-twin-engine-vs-v-twin-engine/
  - Here, Kevin talks about some pros/cons of inline vs v-twins.
  - But it's mostly about packaging, cost, and balancing.
  - He doesn't otherwise suggest that a V-twin could make more low-end
    torque, like you hear a lot of people say.
- Source:
  https://www.cycleworld.com/2016/02/01/motorcycle-v-twin-and-parallel-twin-and-flat-twin-engine-tech-sound-insights/
  - Another good article by Kevin that compares a lot of twin engines.
