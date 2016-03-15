## Ch1: Foundations

Units of electric charge are *coulombs*, which is a certain number of
electrons. A *voltage* is the measure of a *electric potential*. This
is the amount of *energy* (AKA work; measured in *Joules*) required
per unit of electric charge from one point to the other. Sometimes we
call this the *electromotive force* (or *emf*).

You can think of voltage as a measure of how much a unit of charge
wants to be (or doesn't want to be) move to another place.

*Current* is the amount of electric charge passing through a point per
second. Current is measured in amps, which are coulombs per second.

Electrical resistance determines at what rate current will flow
through a component at a given voltage. The most important equation
here is `V=IR`.

*Power* is a measure of the amount of work done per second. The same
amount of energy is used to walk or to run from point A to point B,
but the power is different. Power is an instantaneous quantity (or
averaged over a period of time). The key equation here is `P=VI`; this
makes sense in units: voltage is `J/coulomb` while current is
`coulomb/sec`. The measure of power is `J/sec`, which is called the
*Watt*.

It is typical that `I` is a dependent variable of `V` (which we can
choose by choice of appropriate battery) and `R` (which is a quality
of a circuit). We can rewrite the power equation as `P=V**2/R`. This
makes clear that, if voltage is held constant, but resistance is
doubled, power dissipation is halved. Mathematically this is obvious,
but it violates my intuition.

That seems very odd to me, because an increase it feels like by
increasing resistance, we should increase the amount of work required
to move a unit of charge from one side of the battery to the other. By
analogy, if we try to push a fixed mass across a surface, increasing
the friction of the surface increases the energy cost to move a fixed
distance.

Maybe a useful thought is this: given a 9V battery, you can't attach
it to a very high resistance to get a super hot heating element. Based
on these equations, the energy of waste heat produced per coulomb
should be fixed. All you do by adding a large resistor is slow down
the process.

Real world voltage sources are really like ideal voltage sources in
series with a resistor. Consider a *voltage divider*; this is a
circuit where you have two resistors in series, and attach the output
across the second circuit.

NB: I don't feel like this is getting anywhere. I feel like this book
isn't that good as an introduction, and is really for people who
already have a better intuition about electronics. Maybe I can come
back to it another time.
