## Mass, Gravity, Force

We know *mass*. Mass is how much stuff there is in an object.

We know about *gravity*. Gravity is a *force* exerted between objects on
each other.

    F_g = G m_1 m_2 / d^2

`G` here is the *gravitational constant*.

When we're talking about the gravitational force of the earth on an
object, then we have `G` as a constant (as always), `m_1` is a constant
because of the mass of the earth, and `d` varies little at the Earth's
surface.

Therefore we have `F_g ~ m`, where `m` is the mass of the object. `F_g`
is what gives objects weight.

We know that the application of a force accelerates an object. For a
fixed force, the acceleration varies inversely with the mass of the
object. That is:

    F = ma

This tells us why acceleration of bodies at the earth's surface is
*constant* (does not depend on their mass). Because:

    F_g = ma
    alpha m = ma
    alpha = a

Where `alpha` is a constant. Specifically: `9.8m/s^2`.

The measure of force is the *newton*: the force required to accelerate a
`1kg` mass `1 m/s^2`.

## Energy

We may now ask about *energy*. Energy is consumed to perform *work*. In
this sense, energy and work are two sides of the same coin (and have the
same units).

Our intuition is that (1) it takes twice as much energy to move a 10kg
mass 2 meters up vs 1 meter up, (2) it takes twice as much energy to
move a 10kg mass 1 meter up versus a 20kg mass 1 meter up.

When we perform the work of lifting the mass, we are storing the
energy required to do so as *potential energy*.

What matters is not really the mass, but the *weight* aka *force* that
is resisting us. Thus, we may say that:

    delta E = weight * distance = force * distance

The unit of measure of energy is the *joule*: the energy required to
apply a force of `1N` through a distance of `1m`. Thus the joule is a
newton-meter.

**Wrong intuition**

Imagine a heavy weight suspended from a tree. Imagine you are holding
the other end of the rope. It feels like work right? But is anything
moving? By our equation, shouldn't delta E be zero? Instead, it feels
like delta E should be proportional to `t`, the time you hold the rope!

No. To keep the weight from falling, you must exert an equal and
opposite force. But where does this force come from? It is the result of
bioelectrical activity in your body. To squeeze your muscles and exert
the force takes *energy* from your body. Chemical reactions use stored
energy in your body to maintain the force.

Energy must constantly be expended to maintain the squeeze in the
muscles.

If you were a bolt that the rope were tied down to, then you would be
expending no energy to hold it.

## Energy doesn't determine the force nor the distance (nor duration)

The same amount of energy is required if we want to exert a force of 10N
through 1m as it takes to exert a force of 1N through 10m.

Energy says nothing about how fast it must be released. If energy is
released slowly, it will exert a lesser force, but it will do so for a
greater distance.

We haven't yet said how to know the force applied for what distance.

## Kinetic energy

An interesting note: for the same mass, the velocity it takes on is the
same regardless force/distance combo. We can ignore force and just talk
about acceleration/distance combination, since for a fixed mass this
amounts to the same thing.

    v_final = \int_T a dt = a T

This relies on `T`, which we don't know. However, we do know that the
*average* velocity is

    v_avg = \int_T (\int_t a dt) dt / T

See what I did here? `\int_t a dt` is the velocity at time `t`. Anyway:

    v_avg = (\int_T at dt) / T = (1/2 a T^2) / T = 1/2 a T

We also know that: `d = v_avg T`. Thus:

    d/T = 1/2 a T
    T^2 = 2d/a
    T = \sqrt{2d/a}

Substituting back in, we have:

    v_final = a T = a \sqrt{2d/a} = \sqrt{2} \sqrt{a} \sqrt{d}

What we see here is that doubling `a` at the same time as halving `d`
will cause no difference in the final velocity. Of course, we've assumed
that there is no resisting force. Overcoming a resisting force would not
reduce the energy expenditure, but would reduce the final velocity.

## Kinetic energy

We now see that, if a given energy is imparted to a mass, the mass will
have a determined final velocity:

    v_final = \sqrt{2} \sqrt{a} \sqrt{d}

Let's begin to rework this. First, let's square:

    v_final^2 = 2 a d

Let's replace `a = N/m`

    v_final^2 = 2 N/m d

Last, we can reorganize:

    1/2 m v_final^2 = Nd = E

Thus we have the equation for *kinetic energy*:

    KE = 1/2 m v^2

This is the amount of energy that would have been required to maintain a
force long enough to accelerate the mass from rest to the stated
velocity.

## Power

Power measures

**Whoa**. Need to reflect that:

* Kind of amazing that force affects *acceleration* and not *jerk*. Why
  would this be?
* *Gravitational potential energy* is in a sense a "made up idea."
  * It simply means: how much ability is there to *accelerate* the
    object.
      * You can see that it isn't "intrinsic" to the object, since it
        arrises from interaction between two objects via gravity.
      * If remove second object, "potential" goes away...
  * Anyway, how much ability you have to accelerate is proportional to
    distance. It just is definitionally.
  * Likewise, kinetic energy is meaningless. It is simply implied by
    definitions.
    * If you accelerate an object at rest for a given distance (), you will get an object
    * Since energy is how much ability to accelerate the object

**TODO**: Return to me!

  * In what "sense" is energy stored in an object that is up high?
  * Relationship between velocity and kinetic energy is no more than
    what is implied by definition of force.
  * That is: if energy is

## TODO

* Angular momentum
* Impulse
* Inertia
* Moment of Inertia
* Momentum
* Simple machines:
  * Lever
  * Wheel and axle
  * Pulley
  * Inclined plane
  * Wedge
  * Screw
* Torque
