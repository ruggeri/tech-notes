When you pluck a guitar string, it vibrates. This vibration, if
amplified (maybe by the body of the guitar), creates an audible tone.

An electric guitar does not have a body that physically amplifies the
sound. Metal strings vibrate, and we need to transform that vibration
into an oscillation in electrical potential.

We know that something using induction could work here. We can use a
coil of wire, and if we cause a vibration in the magnetic field, this
should induce a current/voltage in the wire.

If the strings were magnetized, then the movement of the string would
create the magnetic field variation needed. This would induce the
current in the pickup coil.

The strings are metal, but not intrinsically magnetized. Thus, a guitar
pickup features a permanent magnet, the job of which is to magnetize the
guitar string. Now, the vibration of the string can induce a change in
magnetic field, and thus induce a current through the coil.

This explains the purpose of the magnets in the guitar pickup. It
explains the purpose of the coil.

A problem with this design is that if you create a winding of wire with
many turns, this will act as amplifier of magnetic noise. Often, there
is a 60Hz magnetic "hum" that is a result of AC power. This magnetic
variation will be transformed into electrical current by the pickup
coil.

We can eliminate the hum using _two_ coils in series. Take two identical
coils: coils A and B. Each coil has corresponding terminals 1 and 2.

You might imagine that we wire terminal A2 with terminal B1. We measure
the the voltage difference across terminals A1 and B2. That would
basically double the windings, doubling the signal, but also doubling
the noise.

That makes sense: we've wired two devices in series, so they should do
double duty.

Instead, we could wire terminal A2 with terminal B2. We measure the
voltage across terminals A1 and B1. The EMF induced between A1 and A2 is
exactly opposite to the EMF induced between B2 and B1. Thus there should
be perfect cancellation and no EMF should be generated across A1 and B1.

That makes sense: we've wired two devices to work opposite each other.
If their effect is "additive," then the two devices should cancel each
other out.

You could say that the coils are hooked up in opposite directions.
Alternatively, you could see the second coil as "reverse wound." It
turns out that the polarity of electromagnetic induction in a coil
depends on the direction in which the wires are wound. In any coil, if
you proceed from one terminal, you will be rotating clockwise about the
coil axis. From the other terminal, you are rotating counter-clockwise.

The sign of the EMF depends on which terminal proceeds clockwise into
the coil, and which proceeds anti-clockwise.

It does not matter which terminal is at the "right" or "left", when
viewed from the direction of magnetic field change. Leftness and
rightness is not invariant to a rotation of perspective about the coil
axis, so it shouldn't matter. Anyway, both terminals can terminate at
the left or right. Or you can switch which terminal is at the left
versus the right by clipping a half turn from one terminal and adding a
half turn to the other terminal. That would change the orientation of
the terminals, but not the inductance.

Likewise, it doesn't matter if the terminals are located "below" or
"above" the coil loop.

Does it matter which terminal is "farther back" and which is "closer" to
the change in magnetic field? No. In fact, we can construct a coil that
is simply concentric turns of wire, rather than helical. In this case,
the terminals need to stick out at 90deg from the coil, but the
induction effect is the same.

Alternatively, we can note that when we use an anti-clockwise helix
rather than a clockwise helix, the EMF across the front and rear
terminal reverses.

My point is this: if we wire a second identical coil in reverse
(switching the role of the two terminals), we could equivalently
consider that we have actually wired in a _reverse-wound coil_ in
series. The two objects are equivalent in terms of electromagnetic
induction.

Okay: we have two coils (oppositely wound) in series. This eliminates
_both_ noise _and_ signal induction. Oops. What do we do?

We need to remember that the EMF is induced in the coil only because the
metal string is itself magnetized by the magnet in the pickup. The key
is that when we reverse the coil, we can also reverse the polarity of
the magnet. This means that the magnetization of the metalic string is
also reversed. Thus, when we pluck the string, the change in magnetic
field is opposite for each coil. But because they are wound in reverse
directions, the same signal EMF is developed across the terminals.

Thus, with this reverse-wound, reverse-polarity humbucker _doubles_
signal, and eliminates noise. The effect can be offered in a single
package. An example is the Gibson PAF humbucker, which is often covered
and common on Les Paul guitars. They can also be uncovered, which
exposes the pair of pickups.

You can also achieve humbucking with two single coil pickups. This is
just the same RWRP built into the humbucker package. Strats are often
wired this way, with the middle pickup set up with RWRP. Positions two
and four put the bridge/middle or middle/neck pickups in parallel, which
has the same humbucking effect.

Some other time maybe I can explore whether series versus parallel makes
a difference for humbucking or tone. One expects the voltage by wiring
into series to be doubled. But I'm not sure what happens when wiring
inductors in parallel?

I'll note that the most common noise to eliminate is 50/60 Hz hum caused
by mains power.

https://www.sweetwater.com/insync/what-are-rwrp-pickups/

## Note

There seems to be some debate about whether the string is magnetized by
the pickup magnet, or whether the presence of ferrous material simply
changes the magnetic field. I think possibly both views are equivalent?

However, it appears on good authority (including Seth Lover, inventor of
humbuckers), that the string really is magnetized. They say you can
magnetize the string and then remove the pickup magnet and the pickup
will still play.

"""
"That’s right, the magnetic field comes up to the stings there and magnetizes the strings. That’s one of the things that most people don’t understand. They figure that string is waving there and cutting the magnetic lines of force. Nuts. That isn’t it. The magnet, all it does is magnetize the string. Now you’ve got a waving magnetic field. And we have a fixed coil with a waving magnetic field to induce voltage. If you want to, take the magnet out. One you’ve magnetized your strings, it will play until the string loses it. Players think the string, the magnetic field from the magnet comes up to the string and by twisting the magnetic flux back and forth that’s what induces the voltage. That’s not what happens. There’s a certain amount of that, but that’s minor. What is happening is you have a magnetic field that is moving back and forth across the coil. And when you move a magnetic field back and forth across the coil you induce voltage. If you move the field up and down it wouldn’t induce any voltage. It’s the motion back and forth across the pickup that does it."
"""
(From Seymour Duncan's interview w/Mr.Lover)

However, I think that's probably a little unfair. The string has
magnetic _permeability_, which means the string is magnetized in the
presence of a magnetic field (the pole magnet). However, the string has
low coercivity: it will not retain this magnetism without the continued
presence of the magnet. Similar to a drill bit: you can not make it a
permanent strong magnet outside the presence of an external magnetic
field.

Thus the presence of the magnet does not really seem redundant. You
couldn't "magnetize" the string and then remove the magnets.

Also, the presence of the magnet in the coil maybe changes the
inductance?

https://www.thegearpage.net/board/index.php?threads/magnetizing-strings.1657784/
https://www.thegearpage.net/board/index.php?threads/magnetizing-strings.1657784/page-2

Seth Lover interview 1978 with Seymour Duncan
http://www.guitarhq.com/seth.html
https://www.seymourduncan.com/blog/latest-updates/seymour-w-duncans-interview-with-seth-lover
