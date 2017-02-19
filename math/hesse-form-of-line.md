Any line can be described as:

    r \dot v = d

What does this mean? Consider a plane in three-dimensional space. The
contours of the plane are lines perpindicular to the
gradient. Therefore, any line can be expressed as the points
perpindicular to a gradient (which is `r` in this case). If `d=0` then
this line passes through the origin, otherwise we push it out by
`d/norm(r)`.

This immediately shows that what matters is the ratio
`d/norm(r)`. Therefore, let us assume `norm(r)=1`. In that case, `r =
(cos theta, sin theta)` for some theta. This gives the form:

    v_1 cos(theta) + v_2 sin(theta) = d

This form of the line is called *Hesse normal form*.

We now consider all lines passing through a given point `x,y`, that
is, the set of points `(theta, d)` where the above equality is
satisfied. Let's think of `d` as a function of `theta`. Then `d` is
equal to

    v_1 cos(theta) + v_2 sin(theta)

This is a linear combination of `cos` and `sin`. Elsewhere in my math
notes, I show that this is also a sinusoidal function. In fact, it has
period `2pi`.
