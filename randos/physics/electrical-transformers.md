## Setup: Mechanical Force/Torque Transformation

We know that voltage is joules per coulomb of charge moved. Power is
joules per second. Amperage is charge per second. Power equals voltage
times amperage.

By analogy, we can say that mechanical force is energy per meter. Power
is still energy per second. Velocity is meters per second. Thus power
equals force times velocity.

We know that we can use gearing to "transform" force and velocity. If an
input shaft connected to a small gear is used to drive a big gear
connected to an output shaft, then a small input force can be magnified
to exert a large output force. However, the distance of motion the
output force moves through will be proportionately smaller.

## Motor-Generator

We want to do a similar thing with electricity. One way is simple: a
motor-generator. You just run a motor, which is attached through gears
to a generator. This basically converts electricity into motion, then
uses our old mechanical gear-method of transformation.

Note that we use electromagnetic induction to convert the electrical
voltage to a change in the magnetic field which then is converted into
motion. And then we repeat the conversions back, after using gear ratios
to convert.

Can we skip the mechanical conversion?

## DC Voltage Boost Converter

Let's say that you want to convert a low voltage source to a higher
voltage. Of course, you cannot create energy; the circuit operating at
the lower voltage $V_0$ will have to run with a power a current $I_0$
such that $V_0 I_0 = V_1 I_1$. That is, if $V_1 > V_0$, then $I_1 <
I_0$ proportionately.

Okay, well how do we do this? Well, like we said, you could run a DC
motor. This is just the opposite of a DC generator, run in reverse. You
typically use commutation.

But we said we wanted to skip the conversion into mechanical power. Can
we? Instead, we can use an inductor. The circuit has voltage pass
through the inductor, and then through one of two paths: directly to the
other battery terminal, or through a load.

The device first simply runs current through the inductor and back to
the battery. There is no current applied to the load. The inductor has a
lot of current running through it, so it stores significant energy in
the magnetic field.

The device is next "switched" to run current through the load, rather
than through the no-load path. The inductor wants to _maintain_ the
current. It starts expending energy that was stored in the magnetic
field to do this. This raises the voltage so that the load resistance
times the constant current equals the higher voltage.

Before the energy stored in the inductor is depleted, we should switch
back to charging the inductor.

Note that you should constantly be switching the switch open and closed.
This means you'll want to be running some kind of **oscillator**. You
don't need AC power, per-se. In fact, your oscillator can produce
frequencies that are much greater than AC power (and generally at much
lower voltage and power).

Last, I believe it is common to include a diode and capacitor. Otherwise
you're going to get _pulses_ of stepped-up voltage DC, rather than
continuously have higher voltage DC.

Source: https://en.wikipedia.org/wiki/Boost_converter

## Analogy: Hydraulic Ram

This is used to run a primitive sprinkler system. You have a source of
low-head water pressure, and you want to spray water higher than the
drop of the water pressure. How do you do it?

Water will be flowing into the device. This will flow out a waste valve.
But there is also a dashpot attached to the waste valve. Flow out the
waste valve will start to push on this pot, which will _close_ the
valve.

When the valve is suddenly closed, there is still momentum of the water
in the input pipe. This causes a hydraulic _shock_ at the valve. That
is: pressure begins to build here as the valve is closed.

The built-up pressure is then used to drive water through an output
valve.

The pressure can be quite a bit higher than what corresponds to the
drop. This is because the input tube can be quite long and wide, so that
it acts as an inductor storing energy in the momentum of the liquid.

Of course, as the flow rate decreases, there is a reduction in momentum
of the water in the input tube, which decreases the pressure. Eventually
a check valve in the output valve closes, and water is delivered to the
waste valve again.

Notice that, similar to the boost converter, there is a pulsing of the
water.

If anything, the hydraulic ram is slightly more complicated, because it
supplies the oscillator in the form of the dashpot.

Source: https://en.wikipedia.org/wiki/Hydraulic_ram
