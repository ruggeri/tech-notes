A grounded plug has three connections:

1. Live
2. Neutral
3. Ground (also called Earth)

The ground is used for, for instance, grounding the case of an
electronic device. No significant voltage will develop on the case,
since any build up of charge would be sucked away to ground.

Thus, if the live wire is connected to the case, the ground wire will
present the lowest-resistance path to ground. If you touch the case, you
should not be shocked.

A GFCI switch (ground fault circuit interrupter) is installed in
bathroom outlets. It measures the current difference between live and
neutral. Above a certain leakage current, the switch will trip. This
trips the switch if current starts running through ground, or even just
through your body to ground (not through the ground connection).

Note that the breaker (or fuse) in the breaker box will switch if the
live current draw is excessive. However, the tolerated current draw is
far above what will be dangerous to a human. The breaker/fuse is there
to stop too much current from running through the wires and starting a
fire.

Note that neutral and ground are both connected to the earth. The
voltage difference across the two plugs should be zero. So why not just
use neutral to "ground" the case?

The point of the ground wire is to give a path to ground from the case
if the case is energized by a loose connection. Imagine that you used a
wire attached to neutral to "ground" the case. Imagine then if the
neutral wire fails. This happens if the neutral fails anywhere in the
return path. Now, if you touch the case, you represent the only path to
ground. You will be shocked by the case with the full 120V.

If the live wire becomes connected to a grounded case, then a large
current should pass and pop the breaker. That's good, because you can
notice the failure and address it.

Thus, if you ground with neutral, you are one failure (neutral is broken
anywhere on the return path) away from a dangerous condition (voltage
develops on the case).

Note: if you grounded with neutral, you'd need to be very confident of
the _polarity_ of the socket. You don't want to "ground" with the live
wire, right? That's the one that will have a positive or negative
voltage relative to neutral/ground/people.

Polarity also matters because you want switches to interrupt the voltage
relative to the human. That way, when you switch a device off, there is
no dangerous voltage present. For instance, if you have a lamp with a
polarized plug, then when switched off, you should be able to stick your
finger in the light bulb outlet.

You may also note that neutral returns may have to pass a fair bit of
current. The neutral return wire has some resistance; thus, there is a
voltage drop across the return that is proportionate to the current
passed. That means that neutral can develop a voltage. A ground wire
should pass very little current, and thus should not develop a voltage
relative to ground.

## Bootleg Ground

- This is when you simply connect the ground hole to the neutral return
  wire.
  - It's something people might do if they don't have ground return
    wires, but need to update to a three-prong grounded outlet for some
    reason.
  - It is against code and unsafe.
- This effectively uses neutral as a ground.
- That will give you a tingle from the case, because there is a voltage
  at the neutral outlet because of the current passing through the
  neutral wire.
- Again, you have to be careful of polarity. If you have reversed
  polarity, then bootleg ground is going to give you a voltage on your
  case.
- They mention that you might not get a shock if you are insulated when
  touching the case. That is: you don't create a pathway to ground.
  - However, they warn that if you create a signal connection between an
    appliance plugged into a reverse-polarity bootleg ground and another
    device plugged into a normal ground.
  - When you make this connection (like between a computer and a
    printer), you may start sending a short circuit current over a path
    that is expected to only have low (signal level) voltage.
  - That will burn out the signal path circuitry, which is not intended
    to carry this current.
- Source: https://www.ecmweb.com/construction/article/20897728/failures-in-outlet-testing-exposed
- Source: https://en.wikipedia.org/wiki/Bootleg_ground

## Sources

https://electronics.stackexchange.com/questions/213479/neutral-vs-ground-wire
https://electronics.stackexchange.com/questions/189867/ac-why-differentiate-between-ground-and-neutral
https://electronics.stackexchange.com/questions/211010/why-dont-we-use-neutral-wire-for-to-ground-devices-and-earth-wire-for-closing-t
https://electronics.stackexchange.com/a/344132
