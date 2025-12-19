## Engine Power

Engines can provide a certain amount of _engine power_. This is
measured in **watts**, or **Joules per second**. A common unit in the
context of engines is **horsepower**. One horsepower is 745 watts.

**Energy/Work**

The engine power describes how much _work_ can be done per second. Work
is measured in **Joules**, which is defined as one **Newton meters**. To
hoist a 1kg object one meter into the air, we must move it through a
gravitational field that wants to accelerate it downward at 9.8m/s^2.
Thus the 'weight' of the object is `9.8kg m/s^2 = 9.8N`, and the work
done to lift it one meter is 9.8J.

Question: can a force of less than 9.8N lift a 1kg mass? If a force less
than 9.8N is applied _directly_ to the mass, then no. But if a 1N force
is applied using a lever with a **mechanical advantage** of at least
9.8x, then the small force can lift the mass.

How? The difference comes from the ratio of the distances the 1N force
travels versus the 9.8N object. If the mechanical is 9.8x, then the 1N
force moves through 9.8m for each 1m the 9.8N object rises. Thus the
energy expended by the force (9.8J) is equal to the energy stored in the
lifted object (9.8J).

## Gears, RPMs, Velocity

When the car is going at velocity `v`, then the wheels must be rotating
a certain rate. This is defined by `\omega = v/r`. This measures
rotation in radians. To measure in rotations per second, divide `\omega
= v/(2pir)`. This relates velocity to wheel RPMs.

The next question is: how does wheel rotation connect with engine
rotation? The simplest possibility is that the wheel is connected
directly to the engine crankshaft (direct drive). In this case, one
engine rotation will correspond to one wheel rotation.

Engines are designed to operate at certain RPMs. This is maybe 600-2k
for most ICEs. 600 RPMs translates to 3600 revolutions per hour.
Consider 15" wheels: multiply `3600*2pi*(15/12)`, further divide by 5280
to convert feet to miles. This works out to at least 53mph.

So how can you run the car at a lower speed? The answer is gearing. Each
revolution of the crankshaft should correspond to only a fractional turn
of the wheels. First gear on a Toyota Camry is 5:1, so at 600RPM maybe
this corresponds to about 10mph.

The sixth gear on a Camry is 1:1, which is called _direct drive_. But
the 8th gear is 0.67:1. At 600RPM, this corresponds to 800MPH. Gears
that are less than 1:1 are called _overdrive_.

All else equal, you would like to run the engine at lower RPM. This is
more fuel efficient, because the engine does not need to run at full
power.

## Torque And Hill Climbing

Imagine trying to climb a hill. If it is a 100% grade (huge!), then this
is a 45deg angle. In that case, the force that wants to roll the car
back downhill is `1/\sqrt{2}` times the weight of the car. To stop
yourself from rolling down hill, you need the engine to apply a force.

On a level surface, the engine does not need to apply any force simply
to continue at constant velocity. That's not true, because of friction
(and aerodynamic drag), but it is sort-of true.

The engine not only has a power rating (measured in kW or hp), but also
a maximum engine torque. If the torque is too low, the force turning the
engine will be too low to turn the wheels and keep the car from rolling
downhill.

The radius of the wheels and the gear ratio both matter. This is why we
talk about engine _torque_ and not _force_. Perhaps unexpectedly, the
bigger your wheels are, the lower the torque "at the wheel." The smaller
the wheel, the less roadway travel per rotation. And the less roadway
travel, the less climbing. Thus: with small enough wheels, you can climb
any hill.

Consider the gear ratio. If the gear ratio is high, like 5:1, then this
is sort of like reducing your wheel radius to a fifth. One revolution of
the motor is only 1/5th of a revolution of the wheel. This is one reason
you will need to be in a lower gear to travel up hill.

## Torque and RPMs and Engine Power

Question: if you are climbing up hill, how fast can you go? If the
torque exceeds what is needed to start climbing, will you be able to
obtain an arbitrarily high velocity?

Answer: no. The reason is that engine power is limited. Torque is in Nm,
so it is, in a sense, the energy that is available per rotation. If you
multiple by revolutions per minute, you get a measure of energy output
per minute. This is returning us to horsepower/kilowatts.

In summary, this explains a practical limit on the speed of a vehicle
during a climb. Climbing takes energy, and the faster you climb, the
more power is required.

## Sources

- https://www.youtube.com/watch?v=mUO5bp-wyLU EE 5 reasons you shouldn't
  care about HP (as much)
  - HP is only a peak number. Yes, it can help you hit high top speed.
    But for acceleration, especially from low speed, it matters where
    torque is made. Electric engines can make lower HP but provide
    instant torque and accelerate faster.
  - You're often traction limited, so excessive HP cannot accelerate you
    faster from low speeds. HP starts to play more of a factor when
    you've accelerated and where you can sustain high torque at high
    RPM.
  - How much torque you can put down depends on RWD/FWD/AWD. The more
    tires that can deliver torque, the better.
  - Most ways to produce more HP involve greater weight (bigger engine,
    turbos). Weight is not really a penalty for top speed (aerodynamic
    drag and HP are primary variables), but it _does_ matter for
    acceleration. So increasing HP might not increase acceleration if
    you must also increase weight. Weight also hurts cornering.
  - Many ways to make more HP reduce responsiveness. Think turbos (with
    lag), or small bore (struggle to breathe at low RPM).
  - His point is: maximum HP doesn't mean maximum fun. You'll neglect
    other important variables: shape of torque curve, weight,
    responsiveness to change in throttle input.
- https://www.youtube.com/watch?v=u-MH4sf5xkY TO REVIEW EE Basic
  HP/torque discussion.
- https://www.youtube.com/watch?v=lt7iUBE3_AE TO REVIEW EE explanation
  focused on why HP/torque curves cross?
- https://www.youtube.com/watch?v=Nbev14oIH6I D4A explains why
  HP, torque are curved.
  - His first point is that when you hit pedal, you open butterfly valve
    to let more air flow to engine. Either (1) the carb will pull more
    fuel with the faster airflow, or (2) the ECU will inject more
  - Basically, you make low torque when you suck in little air (and fuel
    if carbed). And at low RPM, even with appropriately sized or even
    very large valves, it's hard to suck in enough air because the
    piston is not moving fast enough to generate a strong enough vacuum
    to get air flowing.
  - Eventually, piston speed is doing a great job pulling in air, but
    the valves then start becoming the limit which limits top-end power.
    That's why torque will fall off.
  - You can use turbo/super, but since the boost of those is a function
    of RPM, you'll still have a torque curve that ramps up from idle.
    Electric turbo would be a bit of an exception.
  - Discusses why HP will eventually peak post peak torque because HP is
    torque times RPM.
  - Kind of handwaves about designing a race engine meant to make big HP
    and a more usable street engine. The race engine will have large
    throttle bodies and valves, and hit high RPM. The street engine
    wants a broad torque curve. The race engine can be very peaky, but
    that's okay because it will spend most of its time at peak RPM. It's
    a little handwavy because he doesn't really tell you why tuning for
    low RPM torque gives you a _broad_ torque curve.
  - Does discuss why electric motors give back EMF which explains why
    torque eventually falls off for electric motors.
