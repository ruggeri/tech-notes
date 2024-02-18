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

**V4**

- **TODO**

**Future Engines to Study**

- V6 and V8: these are popular in muscle cars.
- I6: Engine with perfect primary and secondary balance.
- I know that D4A has a video about different 6-cylinder types.
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
