We know that `e^x` is defined so that this function has itself as its
own derivative. Another way to say. `exp(alpha * x)` has derivative
`alpha * exp(alpha * x)`. If you interpret `alpha` as an interest rate,
that's how we talk about continuously compounding interest.

`alpha` is telling you how to scale your current position to calculate
your velocity.

What about `exp(i*t)`? How will we choose to define that? Well, we want
our same property to hold. We want the velocity to equal `i * exp(i*t)`.

That says: we want velocity always to be perpendicular to the position,
with equal magnitude. We also know that `exp(0) = 1`.

That describes rotation around the unit circle with constant speed equal
to 1.0. A rotation takes `2pi` seconds. And the position at any time `t`
is given by:

  cos(t) + isin(t)

And that's what `exp(it)` will be defined as. It is *Euler's formula*.

Note that therefore:

    exp(i * pi) = cos(pi) + i sin(pi) = -1

This is Euler's identity.
