## Engine Configuration: Basic Variables

- Displacement: how much volume in all the cylinders. More displacement
  basically means more torque and more power, but bigger engine.
- Number of cylinders
  - More cylinders (at same displacement) generally means more valves
    (better top end breathing), lower piston accelerations forces
    (higher redline, thus higher HP). But more complexity, and often
    worse low end torque because valves are now _too_ big for best low
    RPM breathing/mixing (mostly covered in bore-vs-stroke notes).
    - Typical example is I4 650 supersports, which have same
      displacement as my Ninja 650, but make way more power (though have
      way peakier torque curves), because each cylinder (half the size
      of mine) can rev so much higher.
- Cylinder arrangement and crank geometry
  - More cylinders often allows arrangements that give better balance
    and smoother power delivery, which improves vibrations (important
    for motos), and also lets engine rev higher.
  - Crankshaft angles are a function of cylinder arrangement. Often
    optimizing for best engine balance, sometimes for tire recovery in
    off-road or race applications.
- Per-cylinder displacement
  - In general, bigger cylinder means more torque. Because bigger always
    means that. More fuel and air can be pulled in and combusted per
    revolution.
  - If you want highest HP, you typically go more cylinders to unlock
    more RPM and more breathing. So if you go fewer cylinders for a
    given displacement, you probably care about torque breadth.
  - So you will tend to tune for better torque breadth. You'll probably
    go longer stroke and smaller valves.
    - Classic is a twin 1200 GS. Each cylinder is double the size of
      my Ninja 650, so it makes stonking torque.
    - Exception is probably Ducati V2s which rev very high, and have
      high bore-to-stroke.
- Other major variables
  - Bore vs stroke: see my notes, but we've already touched on this.
  - Compression ratio: always increases efficiency and horsepower, but
    requires higher grade fuel, more stress on engine parts. See notes
    elsewhere.
  - Turbocharging: see elsewhere, but this increases air that flow into
    engine, allowing you to burn more fuel and creating more torque.
    Rare in motos, but common in cars where highest HP is desired (even
    if less responsive/driveable) or non-performance cars just
    optimizing MPG.
  - Fuel delivery: carbs, port, or direct injection? Will build out
    notes elsewhere to explain/explore.
  - Valvetrain: how many valves, how are they timed, how much lift
    (Ducati's desmo tech). Variable timing? How are they opened and
    closed?

## Displacement, HP, Timeline

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

## Balancing Sources

- As of 2025-12-14, it has been a long time since I reviewed these
  resources. But I think I understand this kind of stuff well, so I
  won't re-review.
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
