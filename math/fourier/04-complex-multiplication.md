Up to now we have been considering *real functions*. I would later like
to consider functions that map a time to a *complex number*.

Complex numbers are all of the form `a + bi`. These are like vectors in
`R^2`. We know how to scale these by real numbers, and how to add these
together.

We know that every complex number can be written in polar form:

    r(cos(theta) + isin(theta))

How will we multiply complex numbers? The distributive law says FOIL,
but I want to try to give more intuition.

We know how to multiply `i` by `i`; the whole point is that `i^2 = -1`.
Note that multiply 1 by `i` is a rotation by 90deg, `i` multiplied by
`i` is a rotation by 90deg, et cetera. So this is giving us a hint.

I say that multiplying by `cos(theta) + isin(theta)` is equivalent to a
rotation by `theta` degrees. We know this is true when multiplying `1`
and `i`.

I say: extend this definition of multiplication *as* rotation to all the
complex numbers. That is: multiplying `a + bi` by `cos(theta) +
isin(theta)` is defined to be the theta rotation of `a + bi`.

Rotation is a linear operation. If you break down a number and rotate
the parts and sum, it's the same as rotating the whole. Which basically
says: FOIL holds for rotation.

*Notice*: multiplying `c1` by `c2` is *not* equal to the length of the
projection of `c1` onto `c2`. I was previously confused about that one
time.
