Goal is to convert DC source to AC.

A rough first approximation of AC can be performed by flipping the
orientation of a battery 120 times per second (60Hz).

Once you have alternating current, of course you can step up the voltage
to 120V or whatever you want using a transformer.

You are going to need some kind of timing circuit to electronically
control the switching, presumably using transistors as switches.

How are you going to smooth the output? One first way is to slice up a
cycle into time segments, and close the circuit during a segment in
proportion to the expected voltage at the midpoint of the segment.

Assuming a constant load, the work performed over each time slice would
vary in proportion to the voltage at the midpoint of the timeslice. The
average power delivered is correct during each time slice, even though
the voltage is never really correct at all.

I bet you could further smooth things out with a capacitor...

They also show how you can build a more sophisticated version that
converts from DC to three-phase. That's overkill for my needs right now.

Source: https://www.youtube.com/watch?v=iIqhAX0I7lI
