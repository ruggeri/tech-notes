Consider any quadratic function:

    ax^2 + bx + c

You wish to find the zeros of this function. I've been spending a lot
of time thinking about optimization, so *I* want to find its minimum
(or maximum).

Note, I say that whether this is an up or down parabola depends only
on `a`. The reason is that I know that the first term will always
dominate the others eventually. Therefore, if `a` is positive, we know
that the function will increase more and more as we increase `x`,
overcoming any decrease that might occur from the `bx` term.

Okay. So let's find the minimum. This is easy to do if we rewrite the
function as:

    a'(x-x_0)^2 + c'

Can we always do this? I say sure, and I'll show you by completing the
square. Let's first expand the above equation:

    a'x^2 - 2a'x_0x + (ax_0^2 + c')

Now, I can see here that `a'` must equal `a`, in order for this to be
a proper rewriting of the original equation. And likewise, I see that:

    b = -2a'x_0 = -2ax_0 => x_0 = b / -2a

So I can see so far that I can partial rewrite the original equation
as:

    a(x - (b/-2a))^2 + c'

What is `c'`? Well, from the prior work, we see that

    c = (ax_0^2 + c')
    c' = c - (b/-2a)^2 = c - b^2/4a

So we now have:

    a(x - (b/-2a))^2 + (c - b^2/4a)

Great. This is just formalizing the process of **completing the
square**. This form is the natural form for expressing a parabola, I
say:

1. Start with the parabola with unit curvature centered at the
   origin. This is `x^2`.
2. Next scale the sharpness of the parabola's curvature. This is
   `ax^2`.
3. Next, translate the minimum from the origin. This gives us `a(x -
   x_0)`.
4. Last, translate the parabola up or down, which gives us `a(x -
   x_0) + c'`.

Great. I say this is a hell of a lot more useful to me than the
location of the zeros. So let's bring it on home and retrieve the
zeros from this form.

Basically, I ask: at how many units from the minimum will `a(x - x_0)^2`
equal `-c'`. That means:

    (x - x_0)^2 = -c'/a
    (x - x_0) = sqrt(-c'/a)

But of course, we want the absolute position of the zeros, not their
position relative to the minimum of the parabola. Therefore:

    x = x_0 + sqrt(-c'/a)

But let's expand out this math:

    x = b/-2a + sqrt(-(c - b^2/4a)/a)
      = (-b + 2a sqrt(b^2/4a^2 - c/a)) / 2a
      = (-b + sqrt(4a^2 (b^2/4a^2 - c/a))) / 2a
      = (-b + sqrt(b^2 - 4ac)) / 2a

Note that I added this distance from the origin, but I could also have
subtracted it, so it's really `+/-` the square root.

This is exactly the quadratic formula we know from high school. We see
that it is just a simple result from a two-step process:

1. Figure out a form for the parabola in terms of curvature, minimum
   location, and vertical offset. This is done through *completing the
   square*, which is the real trick.
2. Do a simple calculation of the how far we need to move from the
   origin to reach the zeros.
