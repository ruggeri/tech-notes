## Abuse of Microwave Transformer

I've seen a zany video about how to turn a microwave oven transformer
(really, just a transformer) into a welding machine.

Basically, you just step down the voltage of the input voltage. In the
linked video, they step down from 220V (probably in India) to 14V.

This is India; they may allow up to 20A. That limit is going to be a
function of the gauge of the home wiring. So you can maybe do up to
4400W of power from running the circuit at full out.

In reality, because metal is conductive, if you attach 220V to a metal
wire, it wants to pass more than 20A, and thus the fuse will break.

To avoid this, you can use a transformer to step down the voltage. Here,
he's stepping down to 14V (1/16th the voltage). The current output will
be 16x though. So, if the transformer is going to avoid tripping a 20A
fuse, the secondary winding should hopefully output no more than 320A.

And that could work: 14V, run through a metal wire, might want to do
320A. Which hopefully won't trip the 220V 20A breaker.

The last question, then, is whether 4400W is a lot of power? I think the
answer is yes, because all that power is dissipated exactly at the
joint, where electrical resistance is by far the greatest. The joint
area is small, and all the energy is dissipated as heat there. That
means its temperature rises very quickly, and the metal becomes molten
and thus can be joined.

To learn more, one can read about Electrical Resistance Welding on
Wikipedia. Spot welders are a typical device, and they're basically just
a transformer used in this way.

Source: https://www.youtube.com/watch?v=f63Si6kRG3g
Source: https://en.wikipedia.org/wiki/Electric_resistance_welding
