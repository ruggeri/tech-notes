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

**Single (AKA Thumper)**

- Simpler than multi-piston engines.
- Smaller displacement, usually less power, but also lower weight, and
  smaller volume.
  - All singles on my motorcycle list make less than 50hp.
- You can increase RPM to increase power, but that tends to wear the
  engine out faster.
  - If you do higher RPMs, you need stronger connecting rods, for
    instance. Because the pistons are going to pull trying to tear
    them apart.
  - KTM makes a thumper that does 77hp. But that's not enough for most
    people on the highway.
- Can sometimes get away with air cooling because air isn't blocked
  from any of the cylinders.
- They have pulsing power delivery and a lot of vibration.
  - This is because the engine is not _balanced_. We'll talk more
    about that later.
- To even out power pulsation, they'll use a larger than typical
  _flywheel_. The flywheel stores power as rotational inertia.
  - Note that the pulsation will also get worse as you increase stroke
    or bore. That's another limit on the displacement.
- You can use a counterweight to try to balance the engine. But this
  won't be perfect, because a counterweight spins with the crankshaft,
  and its motion is thus not perfectly opposed to the piston motion.
  - If you use a counterweight that balances 100% of the vertical net
    force, then at 90deg, there will be a net force horizontally.
  - You've just rotated the plane in which the force acts.
  - The best you can do (with a simple counterweight) is to reduce the
    magnitude of the shake by half and constantly rotate the direction
    of the shake.
- To even out vibration, you can start adding _balance shafts_ to
  counteract vibrations. But soon enough it will be easier to build a
  multi-cylinder engine.
- Single cylinder tends to be more fuel efficient. But is that just
  because it is lower power?
- At same displacement, will make more low-end torque than a twin?
  - **TODO**: Why??

**Inline-Twin**

- Also called: "straight-twin," "vertical-twin," "parallel-twin."
- Two cylinders that are mounted side-by-side.
- Typically transverse mounted. Cylinders are side-by-side,
  perpendicular to the bike. Crank runs perpendicular to the bike axis.
  - Only 1 motorcycle ever did a straight twin longitudinal.
- Are typically more compact and easier to design than flat-twins or
  V-twins.
- Most make between 40-80hp. But there are some that go up to 115hp.
- Crankshaft angle
  - 360deg: both pistons move together.
    - Some people use "parallel twin" to refer specifically to this
      crankshaft angle.
    - Firing is offset by 360deg. This is an even firing interval, which
      contributes to smoothness of the engine.
    - However, the forces are not balanced. Both pistons need to be
      pulled by the connecting rod in the same direction at the same
      time. This doubles the vibration.
    - Typical of older British bikes. Modern Kawasaki K800 (which is
      like a retro bike) is the only example manufactured today?
    - You need balancing shafts to cancel out the net forces.
  - 180deg: pistons are always opposite to each other.
    - Some people use "vertical twin" to refer specifically to this
      crankshaft angle.
    - Because mass of pistons is always opposite, there is no net force
      when firing.
    - Typical of older Japanese bikes. Less vibration let them hit
      higher revs and power.
    - Still typical of Kawasaki (including my Ninja 650) and Honda
      twins. They don't make big twins. Also 300cc bikes from Yamaha and
      Suzuki.
    - Firing of second cylinder is 180deg after first, but then you wait
      another 540deg until the first cylinder fires again. This gives
      pulsing power.
    - Because the forces aren't exactly opposite, they will want to
      rotate around their center. They're applying a torque.
    - The rotational vibration also is typically balanced out, but this
      appears to be easier. Also, I think the rotational vibration is
      less.
  - 270deg: pistons are offset by 90deg.
    - Appears to be a newer design.
    - Some people say this ends up being similar to a V-Twin.
    - Typical of Triumph and Yamaha twins. Also the BMW and Aprilia
      parallel twins. Most popular of these three.
    - These makers make somewhat bigger twins than Kawi and Honda, and
      270deg seems to be popular if you're trying to get a torquier
      engine.
    - The Aprilia Tuono 660 has this configuration.
    - Firing interval is 270, 450, 270. This is less uneven than the
      180deg engine.
    - We'll learn that this is the same interval as the 90deg V-Twin.
      Which is why they have similar sound.
    - The primary balance is not perfect. At 45deg you'll hit a max net
      force of 1.5x. But this is less than the 360deg. And you'll also
      have some rotational vibration.
    - These will need to be balanced out.
- Can be prone to vibration either because (1) unbalanced mass in the
  360deg version or (2) irregular firing in the 180deg version.
- Also, in 180deg version, there is rocking, which is the desire to
  rotate.
- The 180deg version can normally do higher revs than the 360deg,
  because of less vibration.
- A helpful source: https://www.youtube.com/watch?v=a9ZFZABaLbg
- Secondary acceleration
  - The geometry of how the piston is connected to the crankshaft
    matters.
  - Imagine rotating the crank at steady RPM. What happens to the
    piston because of the changing geometric relationship?
  - As the piston is pulled away from top, it needs to be accelerated
    because there is movement in the x-direction. This x movement pulls
    the piston down.
  - However, the second-derivative change in the connecting rod motion
    reaches its highest rate at 45deg. By 90deg, the rate of change in
    connecting rod orientation has reversed.
  - At 90deg, it wants to exert an opposing force on the piston. That's
    because the y-change in the crank pin can be explained through a
    smaller piston movement but a larger connecting rod rotation.
  - This explains why secondary vibration is at twice the rate of
    primary vibration.
  - Secondary vibration is smaller than primary vibration.
- Thus, we see that a 270deg engine will have secondary balance.
  - The 180deg and 360deg will both double the secondary vibration.
  - Though we do note that the 270deg will still have a rocking couple
    with respect to secondary balance.
  - But that's even smaller than the secondary balance problem would
    have been.
- In summary
  - 270deg primary imbalance isn't so bad, and not so hard to engineer
    out.
  - Market research seems to show consumers like the imbalance of the
    270deg versus the 180deg.
  - 270deg sounds a lot like a V-Twin, which people like.
  - But all twins have advantage over V-Twin of being easier to fit in
    the bike, and cheaper to manufacturer because can use a single
    cylinder head and valvetrain.
  - 270deg seems to develop torque lower in rev range - why?
- People also talk about pumping losses. The crankcase space (which is
  the inverse of the compression chamber) is constantly changing with
  the 360deg, which is basically efficiency loss, and thus lower gas
  mileage. The 180deg does the best here.

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
- Cylinder firing is offset by 360deg. This gives even power.
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
- Examples of 90deg V-Twins
  - Ducati V-twins is 90deg and mounted with one cylinder pointing
    almost forward; they thus call it L-Twin.
    - They sometimes call this engine "Testastretta 11°"; the 11 refers
      to something totally unrelated to what we're discussing (valve
      overlap).
    - Like all 90deg V-Twins I know, uses a shared crank pin and fires on
      a 270-540 interval.
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
    - Uses a single crank pin. 270-540 firing interval.
    - No rocking couple because of single crank pin.
  - Suzuki V-Twins (SV650 and V-Strom bikes)
    - 90deg angle. Fires on 270-540 interval like every other 90deg
      V-Twin.
  - I think the major downside to 90deg V-Twin is that it has a lot of
    deadspace between the cylinders and it's hard to fit. That's why
    Suzuki might use it on V-Stroms which aren't intended to be that
    maneuverable, or Moto Guzzi might try to mount longitudinally.
    - Note that the Supersport 950 does have a fairly long wheelbase for
      this class of bike.
- Generic 90deg V-Twin Information
  - When left cylinder is at top, right cylinder is at middle. This is
    what you need for a 270-540 firing interval. Note that you achieve
    this with a shared crank pin and a 90deg V angle.
  - You can use a counterweight that is at 180deg from the crankpin.
    This will always rotate opposite the crankpin. This will be
    positioned exactly opposite to the cylinder whenever it fires.
  - Without the counterweight, you will have a rotating imbalance in the
    engine. With the counterweight, you achieve perfect primary balance.
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

**Weird/Other V-Twins**

- KTM 1290
  - 75deg angle.

**Triple**

- Almost always 120deg offset.
- Extremely unusual to have 180deg offset (2 together, 1 apart).
- Will typically be mounted transverse. The headers will look like a
  trident.
- However, you can also have a longitudinal triple, where the
  cylinders run with the bike. This is the configuration of the
  Triumph Rocket 3.
- Most identified with Triumph. But also Yamaha makes some.

**Four cylinder**

- Inline four
  - Flat plane: 180deg offset.
  - Crossplane: each 90deg offset. Only Yamaha on the YZF-R1
    literbike.
- V4

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
