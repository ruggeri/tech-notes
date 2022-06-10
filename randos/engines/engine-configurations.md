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

**V-Twin**

- Ducati (all their twins) and Suzuki (V-Strom 650s and 1050s). Moto
  Guzzi and Harley. The KTM 1290.
  - Almost always transverse mounting.
  - Moto Guzzi mounts them longitudinal. Advantage for air cooling, but
    disadvantage for width.
  - Honda Shadow, Suzuki Boulevard, Kawi Vulcan, and Yamaha Virago,
    cruisers also use V-Twin.
- Ducati V-twin is 90deg and mounted with one cylinder pointing
  almost forward; they thus call it L-Twin.
- Harley: 45deg. Japanese cruisers all ~45-55deg.

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
