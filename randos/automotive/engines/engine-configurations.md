## Engine Power

- Engine displacement
  - Number of cylinders: more means more power.
  - Size of each cylinder
    - Stroke length: how long each piston travels
    - Bore: diameter of cylinder
- Compression ratio
- Maximum RPM
  - Typically can be greater for smaller cylinders. That's because a
    piston head needs to accelerate from high speed to a dead stop, and
    that puts stress on the connecting rods. The connecting rods want to
    be torn apart. The problem is worse at high RPMs, and for heavy
    pistons.
  - So an engine with more but smaller cylinders can achieve higher RPM
    for a given displacement.
- Fueling
  - More cylinders also means more fuel injectors and valves, which
    means you can give more fuel to the engine.

## Engine Configurations

In progress: moving engine configurations to their own documents.

**Flat (Boxer) Twin**

- This is typical of BMW motorcycles. Nobody else does flat engine
  motorcycles.
- Two cylinders are opposed 180 degrees across the crankshaft.
- Typically longitudinal mounted.
  - Means crank runs parallel to bike axis. Cylinders stick out
    perpendicular from the bike.
  - Can do aircooling this way.
  - Low center of gravity.
  - Danger to the cylinders in a collision.
  - I believe that shaft drive is more attractive in this configuration.
- Cylinder firing is offset by 360deg. This gives even power with 180deg
  silence between combustion.
- No net force, so primary reciprocating balance is perfect.
  - But there is some rocking (rotating imbalance), similar to the
    180deg twin, because of the distance between crank pins.
- Secondary reciprocating balance is also perfect, btw.
  - This is because of the opposition of the pistons. This is superior
    to the 180deg parallel twin.
  - There is secondary rotational imbalance, though.
- I believe that if you use a single crank pin (sometimes called a
  180degree V-twin), you can get rid of the rotational imbalance.
- Because of the even firing order, you don't get the cool syncopated
  sound of the V-Twin or 270deg Parallel designs.

**90deg V-Twin**

- Almost always transverse mounted (crankshaft perpendicular to bike
  body; easily drives chain).
  - Exception is Moto Guzzi, which mounts longitudinal (crankshaft
    parallel to bike body).
  - That's an advantage for air cooling, but a disadvantage for width.
  - I believe Moto Guzzi uses a crank shaft.
- Examples of 90deg V-Twins
  - Ducati V-twins is 90deg and mounted with one cylinder pointing
    almost forward; they thus call it L-Twin.
    - They sometimes call this engine "Testastretta 11°"; the 11 refers
      to something totally unrelated to what we're discussing (valve
      overlap).
    - Like all 90deg V-Twins I know, uses a shared crank pin and fires
      on a 270-450 interval.
    - Transverse mounted.
    - No rocking couple because of single crank pin.
      - Well, that isn't true. Even though there's one crank pin, there
        are two side-by-side connecting rods. Which means you have some
        rotational imbalance.
    - Ducati doesn't do any parallel twins. V-Twins are used for their
      smaller displacement bikes.
    - A downside to this design is that you need a longer wheelbase to
      accommodate the horizontal cylinder. That impacts maneuverability.
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
    - Note that the Supersport 950 does have a fairly long wheelbase for
      this class of bike.
- Generic 90deg V-Twin Information
  - When left cylinder is at top, right cylinder is at middle. This is
    what you need for a 270-450 firing interval. Note that you achieve
    this with a shared crank pin and a 90deg V angle.
    - At 90deg, right cylinder has finished exhaust. At 180deg, left
      cylinder is done with combustion, right cylinder is halfway
      through intake.
    - At 270deg, right cylinder has completed intake, and left cylinder
      is halfway through exhaust. At 360deg, left cylinder is done with
      exhaust, and right cylinder is halfway through compression.
    - At 450deg, now we start combustion in right cylinder. Left
      cylinder is halfway through intake. At 540deg, left cylinder is
      done with intake, and right cylinder is halfway through
      compression.
    - At 630deg, right cylinder is done with combustion, and left
      cylinder is halfway through compression.
    - And at 720deg, we are back where we started. Left cylinder starts
      combustion, and right cylinder is halfway through exhaust.
    - This explains the 270-450 firing interval. Notice the uneven 90deg
      and 270deg of silence.
  - You can use a counterweight that is at 180deg from the crankpin.
    This will always rotate opposite the crankpin. This will be
    positioned exactly opposite to the cylinder whenever it fires.
  - Without the counterweight, you will have a rotating imbalance in the
    engine. With the counterweight, you achieve perfect primary balance.
  - Well: I should note that with two crank pins, you'll have a rocking
    couple so not totally perfect primary balance.
  - Also: you will have _secondary_ imbalance, just like with an inline
    twin.
- The alternating longer and shorter intervals between firing give the
  prototypical V-Twin sound which is emulated by 270deg parallel twins.
- Using two crank pins, you can get 360deg and 180deg firing intervals
  like parallel twins.
  - However, this is exceptionally uncommon.
  - Why? Probably you should just build a parallel twin at that point?
  - Certainly you're going to have primary balance problems with the
    360deg and 180deg engines.
  - That's because the net force will pulse either vertically (360deg
    firing interval) or horizontally (180deg).
  - In addition to the rotational imbalance due to using two crank pins.
- **TODO**: What are the pros-and-cons versus a parallel twin with a
  270deg crank angle?
- Some other thoughts
  - https://www.cycleworld.com/2015/12/03/ask-kevin-cameron-honda-africa-twin-parallel-twin-engine-vs-v-twin-engine/
    - Here, Kevin talks about some pros/cons of inline vs v-twins.
    - But it's mostly about packaging, cost, and balancing.
    - He doesn't otherwise suggest that a V-twin could make more low-end
      torque, like you hear a lot of people say.
  - Basically, I don't see anything that suggests that orientation of
    cylinders should really affect power band.

**~45deg V-Twins**

- These are typical of cruiser type bikes.
- Almost all Harleys are air-cooled 45-deg twin engines.
  - There lightest street cruiser bikes use a 60-deg engine.
  - But all others are the classic 45-degree V-Twin.
  - Harley uses fork-and-blade connecting rods, which means the
    cylinders fire in the same plane. So you only need to analyze this
    plane.
  - Sharing a crankpin means that you have a 315-405 firing interval.
    This gives their classic sound.
- In terms of primary imbalance, a 45-deg twin is going to be quite
  similar to a parallel twin with a 360deg firing order.
  - That's because both cylinders are at about TDC at the same time.
  - BTW, typically these bikes are _overbalanced_, to reduce vertical
    vibration by a bit more than 50%, at the cost of more horizontal
    vibration. That's based on the subjective experience of the rider.
- Another note on sound: Harleys crank at lower RPMs, so you can hear
  the syncopation more than Ducatis, which crank at higher RPMs and
  start to sound more droney.
- Japanese Cruisers are ~55deg V-Twins
  - Honda Shadow (52deg), Suzuki Boulevard (C50: 45deg M109: 54deg),
    Kawisaki Vulcan (900: 55deg, 1700: 52deg), and Yamaha V-Star and
    Bolt R Spec (60deg).
  - I think a lot of manufacturers are getting out of the cruiser game
    because demand is drying up.
  - All transverse mounted.
- Firing of 45deg V-Twin is less uneven than 90deg V-Twin.

**Weird/Other V-Twins**

- KTM 1290
  - 75deg angle. Firing interval of 285-435.
  - As discussed previously, a parallel twin with a 75deg crank has the
    same firing interval.

**Triple**

- Typically mounted transverse, and the headers will look like a
  "trident" a bit.
  - Exception is Rocket III which has a longitudinal mounted engine.
- Yamaha makes some triples, but Triumph is most associated with them.
- Typically make about 80-120HP.
- The "typical" setup is 120deg crank offset.
  - Yamaha calls this "CP3" (Crossplane 3). But this is the typical and
    traditional 3 cylinder setup.
  - All Yamaha triples use this setup.
  - The branding matches CP2 (their 270deg twin) and CP4 (a crossplane 4
    engine) that Yamaha sells.
  - The firing interval is an even 240deg. The first cylinder fires,
    then the third, then the second.
    - Note: power delivery is not perfectly even, because there are
      moments when there are no power strokes occurring (180deg to
      240deg, 330deg to 360deg).
    - There is an even 60deg of silence between combustion cycles.
  - Perfect primary reciprocating balance is achieved.
  - I can also prove that secondary reciprocating balance is perfect.
    - Angles of 0,120,240 correspond to doubled angles of 0,240,120.
    - Thus, if we double the frequency, these angles get swapped, but
      their relative position stays the same.
    - Thus, by the same argument that primary forces are balanced, we
      argue that secondary forces are balanced.
    - Note that it doesn't work for tertiary forces; 0,120,240 become
      0,0,0 in the tripled frequency domain. Thus, tertiary imbalance is
      magnified by 3x in this engine.
  - A 120deg triple will have some primary rotational imbalance.
    - The pivot point actually moves. And the magnitude of the
      rotational imbalance changes throughout the combustion cycle.
    - It is zero when central piston is at TDC.
    - But it is maximum when piston 1 (or 3) is at TDC. But note that
      the pivot point is _not_ at the center piston. It is in between
      the first and center piston.
    - It appears that a balance shaft is needed to even this out.
- Triumph
  - Trident 660, Speed Triple, and Street Triple have crank angles of
    120deg. We've already discussed that setup.
  - Tiger 850, 900, 1200 have T-Plane.
  - T-Plane has crank angles of 0deg, 270deg, and 180deg.
  - Thus fires cylinder 1 at 0deg, cylinder 3 at 180deg, and then
    cylinder 2 at 450deg.
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
  - Another benefit is that the soundtrack is nice.
  - For this reason, twins with 270 crank throws are also popular
    off-road.
  - https://www.youtube.com/watch?v=vU7faKiQleM

**Inline Four Cylinder**

- Generally used for high revving, high horsepower machines.
- Kawisaki, Honda, and Suzuki all make supersports. Also BMW.
  - Ducati and Aprilia prefer V4s, which we'll talk about later.
  - Triumph builds threes.
- Fours generally are built when you want at least 100HP. Otherwise, you
  can achieve it with a twin.
  - To get this, presumably you're normally relying on the high revving
    of the engine to achieve greater HP. Thus, you'll tune it to produce
    peak torque high up in the rev range.
- Your typical design will have each cylinder offset by 180deg.
  - Thus, your firing interval is an even 180deg.
  - This should have perfect primary reciprocating balance, like the
    inline twin.
  - I believe you can eliminate the rocking, if the cylinders are at
    0deg, 180deg, 540deg, 360deg. They should have no moment about their
    center of mass.
    - The firing order is thus: 1-3-4-2.
    - You could easily design an engine which fires 1-2-4-3, which has
      been done. I think it would basically give the same result.
  - I believe you will still have secondary imbalance. This should be
    doubled.
    - I believe balance shafts are used on higher displacement engines
      for this reason.
- There is always one cylinder firing, but there is some pulsation in
  power delivery. That happens because the end of the power stroke
  delivers less power than the beginning.
  - But I think this is common to all engines with no more than 4
    cylinders.
  - Because there is always a cylinder firing, the exhaust note is very
    even. This gives a screaming sound at high RPM.
- Crossplane Design
  - Yamaha is the only one currently doing this for inline fours.
  - Crank angle is 90deg. So pistons are at 0deg, 270deg, 90deg, 180deg
    with respect to each other.
    - Cylinder 1 fires at 0 deg, cylinder 3 at 270deg, cylinder 2 at
      450deg, and cylinder 4 at 540deg.
    - Firing intervals are 270-180-90-180...
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
  - Basically, this like the bike will have more vibration, and this
    really only benefits you when pushing to the absolute limit in
    cornering.
  - Source: https://www.youtube.com/watch?v=uM-ycHS9uvw&t=180s

**Flat Four**

- This is also called a Boxer 4.
- It was used on 1974-1987 Honda Gold Wing.
- It should have primary and secondary reciprocating balance, just like
  the boxer twin.
  - However, there is an improvement here over the I4, because the I4
    does not have secondary reciprocating balance.
- Has an even firing interval of 180deg. Fires 1-3-2-4. That is, the
  left bank of cylinders (which are at 180deg from each other) fire,
  then the right bank of cylinders.
- Next we consider primary rotational balance.
  - We know that the boxer twin does not have primary rotational
    balance, because of the offset of the pistons, which do not share a
    crank pin.
  - However, I believe that each pair of boxers in the flat four will
    want to rotate opposite the other.
  - Thus, there is no net rocking of the engine.
  - Driving 4 Answers agrees with my analysis.
  - In this respect, the flat four is better than the boxer twin. It
    equals the I4.
- However, the secondary rotational balance problem will still be
  present. That's because, when separated by 180deg, the two cylinder
  banks will want to rotate in the same direction. So they double the
  secondary vibration.
- Source: https://www.youtube.com/watch?v=TQlyS2rw-sk

**V4**

- **TODO**

**Future Engines to Study**

- V6 and V8: these are popular in muscle cars.
- I6: Engine with perfect primary and secondary balance.
- I know that D4A has a video about different 6-cylinder types.
- V10: Sounds like it is a very unpopular choice?
- V12: Two I6s. I think just provides more even power delivery (because
  more cylinders). Also, smaller cylinders means can rev higher at same
  displacement. More power.
  - This is the biggest engine typically used in cars.
  - Supercars would have this engine. Very smooth and very powerful.
  - Obviously, complicated.
  - Can't do I12, because too long (and torsion along crankshaft). Also,
    the car can't fit it.
- Flat 12: Compare to V12?

## Displacement and HP

- Here are some references.
- 1L motorcycles with 4 cylinders can make like 175-200HP. Those are
  really fast bikes.
  - They may also weigh about ~400lbs (like my Ninja 650, which makes
    about 65HP).
- A Toyota RAV4 Prime makes ~300HP. However, it weighs about 4,235 lbs.
  - Thus the sport bike has about a 5x advantage in power-to-weight.
- Here are some historical references:
  - In 1963, first 425HP car (a Plymouth).
  - In 1988, first 500HP car (Porsche 959 S).
  - In 1992, first 600HP car (McLaren F1).
- Today, about 600HP is typical of a powerful V8 sports car.
- But at the same time, as cars got a lot more powerful, they were also
  getting a lot heavier.
  - Original 1989 Miata made 115HP but weighed only 2,210lbs.
- A very big automotive engine might be 6L. This compares to motorcycles
  which top out about 1L.
- Another change is that cars have become turbocharged. This improves
  efficiency and emissions, which matter a lot more today. But they also
  change the responsiveness of the engine (turbo lag).
- Last, manual transmission cars are being phased out because people
  don't want them.
  - DCT used to be the performance option, but it looks like higher end
    sports cars are more typically using automatic transmissions.
  - I think maybe automatic transmissions have gotten better, and they
    maybe are less of a hassle to maintain than DCT?

## Sources

- https://www.youtube.com/watch?v=9Bdc9CuBOzc
  - Explains primary reciprocating balance well.
  - From Engineering Explained.
  - Primary reciprocating balance is about the net momentum change in
    the system. If the total translational momentum is constant, the
    engine is balanced.
  - At the end, mentions that a counterweight can balance a
    single-cylinder engine in the vertical plane, but at the cost of
    horizontal imbalance.
- https://www.youtube.com/watch?v=gdHQ8aTfiQQ
  - Explains secondary reciprocating balance.
  - From Engineering Explained.
  - This is a good explanation of secondary reciprocating balance.
  - It shows the geometry: that a piston travels more than half of the
    way down from top dead center to 90deg.
  - It then travels _less_ than half of the way down from 90deg to
    bottom dead center.
  - Clearly cylinder head position (and velocity, and acceleration) is
    not going to be perfectly sinusoidal.
  - **TODO**: I think I need a better/more mathematical explanation for
    why change in effective crank length has period 180deg.
- https://www.youtube.com/watch?v=a9ZFZABaLbg
  - Driving 4 Answers.
  - Explains various parallel twin crank angles. Talks about primary and
    secondary balance.
  - Also explains rocking couples.
- https://www.cycleworld.com/2016/02/01/motorcycle-v-twin-and-parallel-twin-and-flat-twin-engine-tech-sound-insights/
  - Explains all sorts of balancing techniques/compromises in different
    engines. Very useful, but a bit fast, and with no diagrams.
  - Mostly focused on _primary balance_.
  - Does also discuss different sounds created by different firing
    intervals.
- https://www.youtube.com/watch?v=E1_9MrHzZ98
  - Compares V-Twin angles
  - Driving 4 Answers
  - Basically, says that 90deg can achieve better balance, but 45deg has
    more even firing interval and can be more easily fit without
    expanding the wheelbase.
- https://www.youtube.com/watch?v=Oc5_B4Qua_I
  - Engineering Explained.
  - Explains three cylinder engine.
  - Explains crank angles, firing interval.

## Cylinders

why do single cylinders have more torque and less top-end power?

Why might inline twin have less torque than

Triple is a good middleground between twins and fours.
Tiples don't have as much torque as v-twins, but not as much power as inline fours.

Four cylinders have smooth power delivery, can put up high horsepower numbrs
Not much power at bottom end
Sound is high pitched scream

V-Twin
Great low end torque
Sounds good

V-Four
Great torque and power, good sound exhaust

Flat Twin
Great torque

---

A major factor appears to be stroke and bore.

The stroke length, as we have seen, is determined by the radius of the
crank. We know that if you apply the same force at a greater distance
from the crank, you will impart more torque on the shaft. Thus, longer
stroke means more torque on the crankshaft.

On the other hand, the larger the bore (diameter of cylinder), the more
fuel can be put in the combustion chamber. This means more fuel is
burned every cycle, which means more energy is delivered each cycle.

More cylinders means a few things:

- More intake ports for air. Less time to fill the cylinder with air.
- Shorter stroke for pistons to travel. That means that the speed is
  slower. Which means that at same RPM, forces on conrods can be less,
  which stresses them less.
- Also, and probably more important, the pistons are smaller, which
  means they require less force to turn around and stop.

Fewer cylinders at same displacement means:

- Each stroke of power burns more fuel, and applies greater torque.

There is also the _timing_ of the firing. A four cylinder will always be
firing a cylinder, so there is consistent work being performed. Both a
V-Twin and a parallel twin have intervals during which no expansion is
occurring and thus no power is delivered.

- Someone else says that it's a matter of tuning? That the V-Twin can
  only reach 6k RPM, so you tune it for more torque down low?
  - On the other hand, with an inline 4, since you can get it to do high
    RPM, you want to tune it to produce the most torque there.
  -

https://www.reddit.com/r/motorcycles/comments/1v9uw2/why_do_v_twins_tend_to_have_comparatively_higher/

- https://www.youtube.com/watch?v=UV3RwBPqznU
  - Engineering Explained.
  - Bore vs Stroke
  - Oversquare = short stroke/larger bore, undersquare = longer
    stroke/smaller bore.
  - It seems like it shouldn't make a difference? Isn't just engine
    displacement what matters?
  - He seems to indicate that the limitation is on piston speed. That
    would suggest a shorter stroke will allow you to achieve higher RPM.
  - But isn't the limitation on _force_ on the conrod? I believe I
    showed this should be constant for a given displacement and RPM.
  - With a shorter stroke, you can travel the necessary distance in a
    shorter period of time (assuming maximum piston velocity is held
    constant). If piston velocity is the problem, then you'll achieve
    higher RPM for the same piston velocity if the stroke is shorter.
    - Maybe velocity is a problem: like from friction on the sides?
  - Certainly you can fit larger valves on a larger bore.
    - **TODO**: He notes that large valves might be bad at low RPM. Why?
  - He notes that the shape of the volume left for combustion at
    ignition is most square shaped for oversquare designs. This means
    that there is the least surface area for transmitting heat. That
    means more energy is turned into expansion of the cylinder, which
    means higher efficiency.
  - He also notes that it's easier to burn all the fuel quicker in a
    more cube shaped volume because the flame front can travel out in
    more directions more easily. This is preferable because you want
    maximum force to be generated from the very top.

Wait.

Consider the position of the piston:

```
cylinder_displacement = pi * (bore_diameter / 2)^2 * stroke_length
      ~ bore_diameter^2 * stroke_length

position = stroke_length + 1/2 * stroke_length * cos(omega t)
velocity = 1/2 * stroke_length * omega * cos(omega t)
acceleration = 1/2 * stroke_length * omega^2 * cos(omega t)

RPM = omega / 2PI

max velocity ~ stroke_length * RPM
max acceleration ~ stroke_length * RPM^2

## Assumption about piston mass
m ~ pi * (bore_diameter / 2)^2

## Let's consider the centripetal force required to keep the piston
## spinning at a target RPM.
max F = m * max acceleration
      ~ bore_diameter^2 * stroke_length * RPM^2
      ~ cylinder_displacement * RPM^2

## Thus, the force to make a piston travel at a target RPM does not
## depend on stroke or bore, but only the displacement.
##
## If we are worried about force breaking the connecting rod, we can see
## that twiddling the stroke/bore ratio does not matter.

## However, we can also see that decreasing a cylinder's displacement
## will allow it to rev to higher RPM.
```

- Source: https://www.cycleworld.com/horsepower-vs-torque/
  - A very useful source.
  - Says that touring bikes have no valve overlap. That basically means
    that they aren't open long enough to let enough air in at high revs.
  - Says that longer valve timings are better for higher RPM bikes so
    that they have more opportunity to fill up with air.
  - What happens when intake valve is open during compression? At low
    RPM, this is bad. Some fuel is pumped back out by compression. That
    lowers total fuel for ignition, and thus lowers torque.
    - But at high RPM, there is actually substantial intake velocity.
      That creates momentum into the cylinder even despite the rising
      piston.
  - Thus, if tuned this way, engines designed to rev at high RPM can
    produce their most torque at highest RPM, which is how you maximize
    horsepower.
  - This is why variable valve timing is an exciting technology.
    - They can get more torque out of low end without sacrificing so
      much at high end.
- https://en.wikipedia.org/wiki/Valve_timing
  - As discussed, valve timing makes a difference for where an engine
    produces the most torque.
- The location of peak torque is determined by where the engine breathes
  best. The amount of torque depends on how much fuel is filled.
  - Torque is clearly related to HP, but it's RPM neutral.
  - You want to maximize torque within the RPM that your bike is
    intended to operate at.
- Some people claim that a longer stroke will produce more torque?
  - This is the idea of an undersquare engine.
- I believe another factor is the valve size. Basically, one would
  expect that larger is always better.

- What limits redline?
  - Valve float. Valves can't close fast enough. This is going to start
    hurting output/emissions, and also threatens a collision with the
    piston in an interference engine.
  - People do say that longer stroke engines can't do higher RPMs
    because the piston moves at a higher velocity. But isn't it the
    _acceleration_ that matters?
  - They also mention that there will be a breathing problem at higher
    RPM.
  - I mean, we know that displacement will limit redline. But that
    doesn't care about the length of the stroke.
- https://www.cycleworld.com/story/blogs/ask-kevin/motorcycle-redline-determines-horsepower/

  - This lists valve train then connecting rods.
  - He mentions that load is proportional to RPM^2 and piston stroke.
  - But why does the piston mass not matter?
  - He does note some problems with large bores: cooling the large
    pistons, and flame propagation (I guess it's harder to propagate
    flame through a small pancake).
  - Larger bore allows for larger valves.

- TODO: https://www.youtube.com/watch?v=PXD_AvKbCMg
  - Talks about why 4 valves better than two. Might explain valve sizing.
- https://www.youtube.com/watch?v=u4AfvfHqCBM
  - Does not expand on common claim that piston velocity is limited. But
    why?
  - Explicitly says that both undersquare and oversquare will produce
    same torque.
    - It claims that force depends on pressure on piston crown times
      area.
    - Pressure is presumably constant maximum since otherwise would
      crack piston.
    - You can increase the crank radius, but for same displacement that
      comes at cost of decreased bore AKA area of piston. Thus, there is
      less total force that can be exerted on the smaller piston.
    - So everything cancels and you get a torque that is the same.
    - Doesn't that suggest you need less fuel to produce same torque?
    - Also, how does that jive with my previous assumption that fuel
      consumption would be same per cycle but energy from fuel would be
      same, and torque is energy per rotation?
    - I think you need the same total energy, because you need to
      sustain the same pressure through a longer distance.
  - He contradicts Engineering Explained. He claims that long stroke
    will have more heat loss, because of larger area swept by piston.
    Whereas, EE claims that what matters is the surface area available
    for ignition at TDC.
    - I think he's wrong. I think that probably the greatest heat is at
      TDC, when long stroke has the advantage in area.
    - But I do think you can add more fins onto a long cylinder to
      improve cooling.
  - He notes that cooling is more difficult for larger moving parts
    required for big bore engines.
  - Notes that you can get better emissions if the combustion chamber is
    cooler. Which can be accomplished by undersquare engines.
  - Might be worth noting that cooling is a problem for air-cooled
    engines like Harley.
- https://www.cycleworld.com/story/blogs/ask-kevin/how-motorcycle-cylinder-bore-stroke-affect-engine-performance/
  - Kind of all over the place. Does mention that undersquare engines
    have the advantage of being able to rev higher, and also to breathe
    at higher RPM (bigger bore means more valve space).
  - Does note that larger bores are harder to cool.
  - Mentions that long stroke doesn't really increase torque. Small
    bores meant small valves. These small valves can fill the cylinder
    best at low RPM.
  - Question though: why would large valves _not_? I think it's because
    of the _velocity_ of the air in the cylinder, which is actually
    important.
  - **TODO**: It seems really clear that I wish I understood why bigger
    is not always better for valve sizing (and lift). I think that for
    low-end torque, you don't want them too big.
