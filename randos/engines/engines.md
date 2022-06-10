# Engines

This is a random and incomplete source of information about engines.

## Engine Power

Engines can provide a certain amount of *engine power*. This is
measured in **watts**, or **Joules per second**. A common unit in the
context of engines is **horsepower**. One horsepower is 745 watts.

**Energy/Work**

The engine power describes how much *work* can be done per second. Work
is measured in **Joules**, which is defined as one **Newton meters**. To
hoist a 1kg object one meter into the air, we must move it through a
gravitational field that wants to accelerate it downward at 9.8m/s^2.
Thus the 'weight' of the object is `9.8kg m/s^2 = 9.8N`, and the work
done to lift it one meter is 9.8J.

Question: can a force of less than 9.8N lift a 1kg mass? If a force less
than 9.8N is applied *directly* to the mass, then no. But if a 1N force
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
of the wheels. First gear on a Camry is 5:1, so at 600RPM maybe this
corresponds to about 10mph.

The sixth gear on a Camry is 1:1, which is called *direct drive*. But
the 8th gear is 0.67:1. At 600RPM, this corresponds to 800MPH. Gears
that are less than 1:1 are called *overdrive*.

All else equal, you would like to run the engine at lower RPM. This is
more fuel efficient, because the engine does not need to run at full
power.

## Torque

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
talk about engine *torque* and not *force*. Perhaps unexpectedly, the
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
