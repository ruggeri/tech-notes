## Inline Twin

- Also called: "straight-twin," "vertical-twin," "parallel-twin."
  - Some people associate these names with different configurations of
    inline twin, but there isn't a universally agreed nomenclature.
- Two cylinders that are mounted side-by-side.
- Typically **transverse** mounted. Cylinders are side-by-side,
  perpendicular to the bike. Crank runs perpendicular to the bike axis.
  - Only 1 motorcycle ever did a straight twin longitudinal.
- Are typically more compact and easier to design than flat-twins or
  V-twins.
  - Only one casmshaft and one cylinder head for instance.
- Most make between 40-90hp. But there are some that go up to 115hp.
  - I'm talking motorcycles here.
  - These are really too small for cars. Also, I think you'd always have
    more space for a bigger engine on all but the smallest autos.
  - Also, cars need more HP because they're heavier.

**Crankshaft Angle**

```
# 360deg crank
# Crank journals are offset by 0deg. Pistons move together.

-   0deg - 180deg: Piston 1 combustion,  Piston 2 intake      (power)
- 180deg - 360deg: Piston 1 exhaust,     Piston 2 compression (silence)
- 360deg - 540deg: Piston 1 intake,      Piston 2 combustion  (power)
- 540deg - 720deg: Piston 1 compression, Piston 2 exhaust     (silence)

# Note: Alternates between 180deg of power and 180deg of silence.

# 180deg crank
# Crank journals are offset by 180deg. Pistons move in opposite direction.

-   0deg - 180deg: Piston 1 combustion,  Piston 2 compression (power)
- 180deg - 360deg: Piston 1 exhaust,     Piston 2 combustion  (power)
- 360deg - 540deg: Piston 1 intake,      Piston 2 exhaust     (silence)
- 540deg - 720deg: Piston 1 compression, Piston 2 intake      (silence)

# Note: Alternates between 360deg of power and 360deg of silence.

# 270deg crank
# Crank journals are offset by 90deg. Piston movement is out of phase by 90deg.

-   0deg -  90deg: Piston 1 combustion A,  Piston 2 intake B      (power)
-  90deg - 180deg: Piston 1 combustion B,  Piston 2 compression A (power)
- 180deg - 270deg: Piston 1 exhaust A,     Piston 2 compression B (silence)
- 270deg - 360deg: Piston 1 exhaust B,     Piston 2 combustion A  (power)
- 360deg - 450deg: Piston 1 intake A,      Piston 2 combustion B  (power)
- 450deg - 540deg: Piston 1 intake B,      Piston 2 exhaust A     (silence)
- 540deg - 630deg: Piston 1 compression A, Piston 2 exhaust B     (silence)
- 630deg - 720deg: Piston 1 compression B, Piston 2 intake A      (silence)

# Note: Uneven interval of silence between power pulses. 90deg then 270deg of silence between pulses.
# Note: This is the same timing as a 90deg V-twin. The exhaust note should be the same.
```

- 360deg: both pistons move together.
  - Some people use "parallel twin" to refer specifically to this
    crankshaft angle.
  - Firing is offset by 360deg. This is an even firing interval, which
    contributes some smoothness to the engine.
    - 180deg of power, 180deg of silence, 180deg of power, 180deg of
      silence.
  - However, this isn't a smooth engine. The forces are not balanced.
    Both pistons need to be pulled by the connecting rod in the same
    direction at the same time. This doubles the vibration.
  - Typical of older British bikes. Modern Kawasaki K800 (which is
    like a retro bike) is the only example manufactured today?
  - You need balancing shafts to cancel out the net forces.
  - This is really a classic British design, and just not used today.
- 180deg: pistons are always opposite to each other.
  - Some people use "vertical twin" to refer specifically to this
    crankshaft angle.
  - Because mass of pistons is always opposite, there is no net force
    when firing.
    - That is, the engine has primary reciprocating balance. We'll see
      later that it doesn't have secondary reciprocating balance.
  - Typical of Japanese bikes (at least before the current 270deg
    crankshaft vogue). Less vibration lets them hit higher revs and
    power.
  - Still typical of Kawasaki (including my Ninja 650) and Honda
    twins. They don't make big twins. Also 300cc bikes from Yamaha and
    Suzuki.
  - Firing of second cylinder is 180deg after first (0deg of silence),
    but then you wait another 540deg (360deg silence) until the first
    cylinder fires again. This gives pulsing power.
    - 180deg of power, 180deg of power, 360deg of silence.
    - This is a very uneven firing interval.
  - Because the forces aren't exactly opposite, they will want to
    rotate around their center. They're applying a torque.
  - The rotational vibration also is typically balanced out, but this
    appears to be easier. Also, I think the rotational vibration is
    less.
- 270deg: pistons are offset by 90deg.
  - Sometimes called "crossplane" because crankpins don't lie in a plane
    with the crankshaft; they're offset by 90deg.
  - Firing interval is 270, 450, 270. This is less uneven than the
    180deg engine.
    - 180deg of power, 90deg of silence, 180deg of power, 270deg of
      silence.
  - We'll learn that this is the same interval as the 90deg V-Twin.
    Which is why they have similar sound.
  - Appears to be a newer design.
  - Some people say this ends up being similar to a V-Twin. Especially
    in sound. That makes sense, because sound comes from exhaust and is
    largely a function of firing interval.
  - Typical of Triumph and Yamaha twins. Also the BMW and Aprilia
    parallel twins. Most popular of these three.
    - The 270deg twin is becoming very popular. I think it's also
      because it has the sound people like, but more practical to
      package and maintain than a V-twin.
  - These makers make somewhat bigger twins than Kawi and Honda, and
    270deg seems to be popular if you're trying to get a torquier
    engine.
    - **TODO**: Is this true? Who says that a 270deg is torquier? Is
      there a source for that?
  - The Aprilia Tuono 660 has this configuration.
  - The primary balance is not perfect. At 45deg you'll hit a max net
    force of 1.5x. But this is less than the 360deg. And you'll also
    have some rotational vibration.
  - These will need to be balanced out.
- KTM 890 engine has 75deg crank angle and 285-435deg firing interval.
  This is equivalent to a 75deg V-Twin with a 360deg firing crank
  angle (the setup of the KTM 1290 engine).
  - **TODO**: Why did they choose this?

**Secondary Vibration**

- The geometry of how the piston is connected to the crankshaft matters.
- Imagine rotating the crank at steady RPM. What happens to the piston
  because of the changing geometric relationship?
- As the piston is pulled away from top, it needs to be accelerated
  because there is movement in the x-direction. This x movement pulls
  the piston down.
- However, the second-derivative change in the connecting rod motion
  reaches its highest rate at 45deg. By 90deg, the rate of change in
  connecting rod orientation has reversed.
- At 90deg, it wants to exert an opposing force on the piston. That's
  because the y-change in the crank pin can be explained through a
  smaller piston movement but a larger connecting rod rotation.
- This explains why secondary vibration is at twice the rate of primary
  vibration.
- Secondary vibration is smaller than primary vibration.
  - Well, perhaps in theory it could be greater. But it's smaller
    because the accelerations of the pistons due to the secondary effect
    of connecting rod position are smaller.
- The 180deg and 360deg will both double the secondary vibration.
- Though we do note that the 270deg will still have a rocking couple
  with respect to secondary balance.
- But that's even smaller than the secondary balance problem would have
  been.

**Pumping Losses**

- People also talk about pumping losses. The crankcase space (which is
  the inverse of the compression chamber) is constantly changing with
  the 360deg, which is basically efficiency loss, and thus lower gas
  mileage. The 180deg does the best here.

**Summary**

- Inline twins can be prone to vibration either because (1) unbalanced
  mass in the 360deg version or (2) irregular firing in the 180deg
  version.
- Also, in 180deg version, there is rocking, which is the desire to
  rotate.
- The 180deg version can normally do higher revs than the 360deg,
  because of less vibration.
- 270deg primary imbalance isn't so bad, and not so hard to engineer
  out.
- Market research seems to show consumers like the imbalance of the
  270deg versus the 180deg.
- 270deg sounds a lot like a V-Twin, which people like.
- But all twins have advantage over V-Twin of being easier to fit in the
  bike, and cheaper to manufacturer because can use a single cylinder
  head and valvetrain.
- 270deg seems to develop torque lower in rev range - why?
  - I'd like to see a source for this. I am not convinced this is true.
- I see lots of bikes (mostly Japanese) up to 650cc class bikes use
  inline twin.
  - Triumph makes a fair number of 900cc and 1200cc twins. I think this
    is for their torquey street bikes. These are probably the most
    attractive inline twin bikes to me, because they are large enough to
    make a lot of torque.
  - Also BMW's smaller displacement bikes (800-900cc).
  - And some adventure off-road bikes (Africa Twin, Tenere).

## Sources

- https://www.youtube.com/watch?v=a9ZFZABaLbg
  - D4A compares 180deg, 360, and 270deg variants.
- https://www.youtube.com/watch?v=w0PBlc1b0vo&t=2s
  - A newer D4A video that explores 270deg vs 285deg inline twins.
