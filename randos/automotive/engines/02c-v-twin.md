# V-Twin

## 90deg V-Twin Firing

```
# Shared crank pin. But bank angle between cylinders is 90deg.
#
# I'll imagine crankshaft spins from left piston to right. Left will
# fire first.

-   0deg -  90deg: Piston L combustion A,  Piston R exhaust B     (power)
-  90deg - 180deg: Piston L combustion B,  Piston R intake A      (power)
- 180deg - 270deg: Piston L exhaust A,     Piston R intake B      (silence)
- 270deg - 360deg: Piston L exhaust B,     Piston R compression A (silence)
- 360deg - 450deg: Piston L intake A,      Piston R compression B (silence)
- 450deg - 540deg: Piston L intake B,      Piston R combustion A  (power)
- 540deg - 630deg: Piston L compression A, Piston R combustion B  (power)
- 630deg - 720deg: Piston L compression B, Piston R exhaust A     (silence)

# Note: uneven interval of silence between power pulses. 270deg then
# 90deg of silence between 180deg power pulses. This is the same as the
# 270deg crank inline twin.
```

## 90deg V-Twin Balance Analysis

- Primary translational imbalance. We need to sum two primary force
  waveforms. The waveforms are (1) vector valued, oriented at 90deg, and
  (2) out-of-phase by 90deg.
  - The primary translational force will continuously rotate. The
    magnitude of the force will be constant, but its direction will
    change. It will be oriented in the direction of the crank journal.
  - This force can be _eliminated_ with a crankshaft counterweight.
- Primary rotational balance
  - Because they share a crank pin, the cylinders are almost in the same
    plane, with force directed radially. Any rotational balance should
    be very minor.
- Secondary translational imbalance.
  - We again need to sum two vector values, each oriented at 90deg.
    However, one is 180deg out of phase with the other.
  - This should shake back-and-forward alone the line that is 45deg from
    each cylinder.
- Secondary rotational balance
  - Again, since the cylinders are almost in the same plane, we do not
    expect a rotational imbalance.

## Counterweight Balancing

- Let's eliminate the primary translational imbalance with a
  counterweight on the crankshaft.
- You want to generate a force that is always equal and opposite to the
  primary translational force.
- Since the primary translational force rotates, you need the balancing
  force to rotate at the same rate and direction.
- That suggests adding a _counterweight_ to the crankshaft. It can be
  opposite the crank pin.
- It should weigh approximately 100% of a piston.
- We know it takes a centripetal force on the counterweight to spin it
  around.
- The counterweight will be opposite each cylinder when the
  corresponding piston is at TDC. That's exactly when the translational
  force points out in the direction of the cylinder.
- If we use a single pair of counterweights straddling the crankpin (and
  the CoM), then they shouldn't cause a torque.
- To balance the secondary forces, we need something that runs at 2x the
  frequency of the crankshaft. We'd need a balancing shaft. I don't
  believe any V-twin bothers with this.

## Mounting

- Almost always transverse mounted (crankshaft perpendicular to bike
  body; easily drives chain).
  - Exception is Moto Guzzi, which mounts longitudinal (crankshaft
    parallel to bike body).
  - That's an advantage for air cooling, but a disadvantage for width.
    Moto Guzzi still makes air cooled bikes, but it's not a modern
    design (lower power and efficiency, though simpler).
  - I believe Moto Guzzi uses a crank shaft.

## Examples

- V-Twins tend to be less common than inline twins.
  - More expensive in parts and maintenance. And 270deg inline twin gets
    the sound people want.
  - And V-twin is harder to "package" (fit into the motorcycle body).
- Harley-Davidson bikes use a narrow V-angle (45deg) and large
  cylinders.
- Ducati focuses on 90deg V-twin and V4s. I think all their bikes are
  V-twins or V4s.
  - One cylinder faces almost forward, and the other up-and-down. They
    call it an L-twin sometimes.
  - They sometimes call this engine "Testastretta 11°"; the 11 refers to
    something totally unrelated to what we're discussing (valve
    overlap).
  - A downside to this design is that you need a longer wheelbase to
    accommodate the horizontal cylinder. That impacts maneuverability.
  - Thus the SuperSport 950 is actually pretty long.
  - MotoGP of today used V4's with one cylinder bank almost parallel to
    ground, and other vertical.
- Moto Guzzi specializes in transverse 90deg V-twin.
- Suzuki makes the V-Stroms and the SV650 with 90deg V-Twin.
- KTM does use V-twins on their 1290 ADV bike. They use 75deg.
  - I think this is a quite oversquare design.
- Honda and Kawasaki don't make any V-twin or V4 bikes. Probably Japan
  doesn't do them in part because they are more expensive.
  - Also I think Japanese bikes were early to use water cooling and
    didn't need the air cooling benefit for V-twins.
- Aprilia makes V4s, but does 270deg inline twins instead of V-twins.
- BMW and Triumph don't do any V-twins.

## Discussion

- I think the major downside to 90deg V-Twin is that it has a lot of
  deadspace between the cylinders and it's hard to fit. That's why
  Suzuki might use it on V-Stroms which aren't intended to be that
  maneuverable, or Moto Guzzi might try to mount longitudinally.
  - Note that the Ducati Supersport 950 does have a fairly long
    wheelbase for an alleged sport-bike.
- A classic stereotype of V-Twins is that they are torquey.
  - That comes from bikes like Harleys.
  - The V-Twin can have wider bore because cylinders aren't
    side-by-side. You can make the cylinders wider without increasing
    bike width.
  - You can also increase _stroke_ if engine is mounted so that
    cylinders are symmetric across vertical. You can increase stroke
    without increasing length nor height too much.
  - So you increase displacement, which is good for power.
  - And this layout favors an undersquare design. That favors tuning
    for broad torque.
  - Also, Harleys were traditionally aircooled so they couldn't make
    make highpower anyway because they couldn't get rid of the heat
    (especially with one cylinder oriented behind the other!).
- So that's where V-twins get a reputation as torquey. It's not really
  anything to do with the cylinder orientation. It's from how packaging
  affects decisions about how much power the bike can make, and the
  stroke/bore.
- An exception is the Ducati L-Twins. Those are 90deg twins with a
  horizontal and a vertical cylinder.
  - With this orientation, you can't really increase stroke without
    increasing engine height. So Ducati has big bore and shorter stroke,
    which is very oversquare. Which favors high RPM performance.
  - Cooling will also be better.
  - But this is Ducati's choice. They want an oversquare design and an
    engine tuned for high RPM engine because they are a more racey
    company.
  - So a Ducati L-Twin is _not_ going to feel like a traditional v-twin.
  - I think the KTM 1290 also is very oversquare, for mostly the same
    reasons. Except it is oriented with cylinders across vertical (and
    with a 75deg angle). Maybe that makes a shorter wheelbase and more
    agile.
- In general: packaging of a V-twin can be difficult. And
  parts/maintenance are going to be more expensive/complicated.

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
  - Anyway, see discussion above.
- I think that the main tradeoff is really about packaging and cost.
  - It's easier to fit an inline twin into a bike.
  - Cost to produce and maintenance are cheaper on the inline twin. You
    only have one case, valvetrain, et cetera.
- OTOH, V-Twin might be narrower (when mounted transversely), because
  it's only one cylinder of width.
  - Though Kevin Cameron says you still need starter and alternator,
    which adds width too.
  - Maybe that's why V-Twin isn't a crazy good idea if all you want to
    do is to increase bore.
- So we see that V-twin could be a natural idea for making a bigger
  displacement engine tune for more/broader torque. But then Ducati
  flips the script by increasing bore without really increasing stroke,
  and tuning for high RPM and power.
  - But at that point, why not go I4?
  - A big reason is: racing regulations used to let you do 1L V-twin
    limit and only 750cc I4 limit.
  - Another reason is that V-Twin lets tire recover to get better
    traction, which means harder acceleration out of corner. Now I4s can
    do this with crossplane cranks...
  - Engine width is narrower which gives better aerodynamics and lean
    angle. And desmo system maybe worked better with just two cylinders
    (I'm not getting into that here).
  - Basically: Ducati's 90deg L-Twin design is a "heritage" of an
    arbitrary rule that they figured out how to take advantage of. It's
    not actually superior on the street to an I4 if you want a sporty
    bike.
  - Note: as soon as rule giving V-Twin a displacement advantage was
    eliminated, Ducati switched to V4.

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
