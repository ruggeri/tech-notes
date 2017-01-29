Say you have a parabolic surface, upturned. You want to find the
minimum.

## Univariate

Let's start with a single dimension. Start at a point `x'`. You want
to find another point `x*` where the derivative is zero. So consider
the derivative at `x'`. Note that because this is a quadratic
equation, the derivative will change at a constant rate because the
second derivative is constant.

Therefore, divide the derivative by the second derivative; call this
`s`. You want to then move `-s` units from `x'` to get to a point
where the derivative has fallen to zero.

Note: the ratio tells you everything you need to know about how far to
move. It tells you nothing about the change in `y` due to the move:
five units away from the minimum will yield a much better improvement
of `y` for `y=100x^2` than for `y=x^2`.

## Sum of Quadratic Functions of Orthogonal Projections

Now let's consider a parabolic surface. Let us say that it is of the
form:

    a_0 + \Sum a_i2 x_i^2 + a_i1 x_i

We see immediately then that we may minimize by separately minimizing
each part of the summation, which is a quadratic function in just
`x_i`, and thus independent from any other part of the sum. This
problem is thus the same as before.

**What Parabolas Are These?**

These parabolas are exactly those where:

1. The contours are elliptical with major and minor axes aligned with
   the coordinate axes, AND
2. The center of the contour may be translated.

Let us start with #1. It is clear that if all `a_i2` are equal and all
`a_i1=0`, then if we try to find the level set for `z`, we have

    \Sum a_2 x_i^2 = z - a_0

This is the definition of a circle (or sphere or hypersphere or
whatever). Since the contours are circles centered at the origin, this
surface will be invariant under rotations.

For the second part, note that

    ax^2 + c

always has its minimum at 0. We may observe how to move the minimum:

    a(x-x_min)^2 + c = a x^2 - 2x_min x + (c + x_min^2)

What this implies is that a quadratic function with a linear term is
always equivalent in shape to a quadratic function *without* linear
term, but with minimum appropriately shifted.

## Sum of Quadratic Functions of non-Orthogonal Projections

Consider a quadratic function like this:

    y = x_1^2 + (x_1 + x_2)^2 = 2x_1^2 + 2x_1x_2 + x_2^2

We see that this involves a cross-term `2x_1x_2`. This means that the
mixed partial `dy/dx_2dx_1` is non-zero. That means we cannot
decompose our move into a move along `x_1` and a move along `x_2`.

My question is this: can we decompose the problem into two seperate
parts, which we can blindly solve without reference to the other, and
combine these solutions in the simple way to produce an answer for
minimization of this function?

The answer is yes. First, let's consider some partials:

    dy/dx_1 = 4x_1 + 2x_2
    dy/dx_2 = 2x_2 + 2x_1
    dy/(dx_1 dx_1) = 4
    dy/(dx_2 dx_2) = 2
    dy/(dx_1 dx_2) = 2

This last partial shows that we cannot blindly decompose into problems
along `x_1` and `x_2`.

Let's ask: is there a vector `v` such that

    dy/(dx dv) = 0

My particular hope is that this `v` is the same everywhere.

The answer to solving this problem is to remember: for a
differentiable function, all tangent lines lie inside the same tangent
plane. Specifically: if `v` is a linear combination of `x_1` and
`x_2`, the partial along `v` is the same linear combination of
partials along `x_1` and `x_2`.

So, what we want is a `v` such that:

    dy/(dv dx_1) = 0

We know that

    dy/(dx_1 dx_1) = 4
    dy/(dx_2 dx_1) = 2

This suggests that `v=(1, -2)` would work, since

    dy/(dv dx_1) = 1(dy/(dx_1 dx_1)) + -2(dy/(dx_2 dx_1)) = 4 + -2*2 = 0

We could normalize `v` to a unit vector, but it doesn't matter.

Note that there is no dependence on position. Therefore, even as we
move along `x_1` to minimize in this axis, this has no effect on the
move that we want to make along `v`, because it has zero mixed
partial.

Let's prove it works! Consider the point `(5, 2)`. Then we have at
this position:

    dy/dx_1 = 4x_1 + 2x_2 = 24
    dy/(dx_1 dx_1) = 4

This says to move backward `6` units along `x_1`. Let's consider what
to do along `v` now:

    dy/dv = dy/dx_1 - 2dy/dx_2 = -2x_2 = -4
    dy/(dv dv) = dy/(dx_1 dx_v) - 2dy/(dx_2 dx_v) = -2 * -2 =4

This says to move one step along `v`. Thus:

    (5, 2) + -6(1, 0) + 1(1, -2) = (0, 0)

Which is indeed the appropriate minimum.

## Question Regarding Ellipses

I originally wondered whether, if the axes of the quadratic were
perpindicular, whether we could correctly calculate the needed
movement from a different perpindicular basis.

This works when the contours are circular, but that is needless to
say, since this surface exhibits rotational symmetry.

This is *not true* for an ellipse. If you try to measure the needed
changes relative to a rotation of the axes of the surface, you will
*not* calculate the correct answer. Here is why:

  v_1 = cos(\theta)x_1 + sin(\theta)x_2
  v_2 = -sin(\theta)x_1 + cos(\theta)x_2

Cool. I'm assuming the axes of the parabola lie along the coordinate
axes. Now let's use some notation:

  d_1 = partial along x_1
  d_11 = second partial along x_1
  etc.

So, let's calculate:

  d_{v_1} = cos(\theta) d_1 + sin(\theta) d_2
  d_{v_2} = -sin(\theta) d_1 + cos(\theta) d_2

Great. Let's next consider the mixed partial!

  d_{v_2 v_1} = -sin(\theta) d_{x_1 v_1} + cos(\theta) d_{x_2 v_1}

Great! So let's calculate these:

  d_{x_1 v_1} = cos(\theta) d_11
  d_{x_2 v_1} = sin(\theta) d_22

Notice the loss of the mixed partials `d_12`.

Therefore, plugging in, we have:

  d_{v_2 v_1} = -sin(\theta)cos(\theta) d_11 + sin(\theta)cos(\theta) d_22

This says that the mixed partial is exactly zero exactly when either:

1. The new axes are perpindicular to the old axes, OR
2. The second partials were equal in the beginning, which means the
   contours were always circular.

My next question is whether the mixed partial effects can cancel
out. But this can only happen exactly when the calculated movement
along `v_1` is exactly opposite the calculated movement along
`v_2`. In that case the second order effects will cancel each other
out.

Everywhere else, we would get the wrong answer.

## General Thoughts

What we've shown is that we can always decompose a quadratic function
into quadratic functions of components not necesarily orthogonal.

I believe this also demonstrates that not all parabolic surfaces have
elliptical contours. That's because an ellipse has perpindicular major
and minor axes.

We've shown that it is simple to reconstruct such a basis. This
actually involves solving a series of linear equations. *There's
probably some algebraic insights to be had here!*

However, for now, I am satisfied with my understanding of quadratic
optimization. What we know is:

1. You can't blindly try to optimize along two coordinate axes.
2. In fact, you often can't optimize along any rotation of the
   coordinate axes.
3. That's because you need to figure out how to undo the "skew" of the
   quadratic function.
