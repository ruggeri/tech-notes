## GFCI, RCD, RCBO

GFCI (ground fault current interrupter) is what we described: if it
detects current that isn't returning via ground, it pops. But it won't
stop _overcurrent_.

RCD (residual current device) is just the European name for GFCI.

You can combine RCD with over-current protection. In this case, it is
called a RCBO (residual current circuit breaker). They usually use a
circuit breaker tripped by a solenoid.

It's common in Europe to put the entire house on an RCD. In America, we
typically just use GFCI on a few outlets. The pro is that you can easily
identify the cause of the ground fault. The con is that fewer outlets
are protected.

## Shaver Sockets

Deionized water is not a very good conductor, though it's not a perfect
insulator (nothing is). Regular water is fairly conductive, and seawater
even moreso.

So, if you drop an unsealed, plugged-in electrical device into a tub of
water, you can draw a large current as a low resistance path between the
two terminals going into the electrical device are bridged by a
low-resistance conductor. You can pop a fuse/flip a breaker, but that
takes some time. And, there is an intermediate range of currents which
are dangerous to humans but not enough to trip the breaker.

In Goldfinger, Bond electrocutes an assassin by throwing a resistance
heater (I thought it was a fan) into a bathtub. Would the guy die?
Probably not; there should be a short and breaker should pop, but
there's no reason that the person should be part of the main electrical
path. If they're just sitting in the tub, they're not part of a path to
ground, merely by being immersed in a liquid, part of which forms a path
to ground. The guy should be fine.

NB: there seems to be a lot of disagreement about this. Some people do
say that you can die and cite news sources of people dying charging
cellphones in the tub (do they mean extension cords? USB only carries 5V
DC, which isn't a lot...). Others are sure you're fine. Mythbusters ran
a test to suggest you die, but I actually think it's a bullshit test.

Anyway, no one would say it's a safe or desirable scenario.

Source: https://www.quora.com/Would-one-be-electrocuted-if-some-electrical-device-falls-into-the-bathtub
Source: https://www.electriciansforums.net/threads/shocking-bathtub-scene-in-the-james-bond-goldfinger-film.198081/

Still, there are dangers. One problem is that wet skin is a much better
conductor than dry skin. Maybe 100-500x more conductive. And if you're
standing a puddle trying to change a lightbulb, that could be dangerous,
if the puddle includes a path to ground.

Source: http://pressbooks-dev.oer.hawaii.edu/buildingmaintenance/chapter/electrical-safety/#:~:text=Skin%20Condition%2D%20The%20human%20body's,)%2D%20150%20ohms%20of%20resistance

So what do these shaver outlets (called Shaver Supply Unit) do? First,
they incorporate an isolating transformer. That means, you can only
complete the circuit by returning to the outlet. You cannot have a
ground fault leak to a common ground.

That's great! But the isolating transformer needs to be built into the
wall, and it can't be too big. Its size can only handle a certain amount
of power; about just enough to run a shaver! A hair dryer, or even a
lightbulb, if you tried to run it through the shaver outlet, would
destroy the transformer by drawing too much current.

Anyway, GFCI is basically the superior device. You can use them like a
normal outlet, with 15A draw max.

Source: https://www.quora.com/Whats-special-about-shavers-only-outlets
