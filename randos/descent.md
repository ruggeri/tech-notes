Let's start off.

Say you're on a surface. Take the derivative with respect to any
vector. If it is non-zero, that means that you can move either in this
direction (or the opposite) some amount `\eps` and go down.

You do not know `\eps`, but you know such an `\eps` exists. If you
like, you can explore different values to see how big a step to
make. If a step is too big, then definitely halve it. If a step does
make the curve go down, maybe try doubling it to see if you can get
further down. And maybe try havling it to see if your step could be
less.

You could have an algorithm that randomly chooses vectors and
continuously tries this. It would work.

## Gradient Descent

Let us say we are taking a step of size `\eps`, for a small value of
`\eps`. We could take this step in any direction, but I would like to
take it in the "best" direction.

Let us consider the first-order approximation of the surface. This is
a plane where the slope is described by the gradient of the surface at
this point.

What is best step on this plane? Clearly it should be "directly
uphill", which is along the gradient. We can prove this, though.

I would like to do this two ways. The first appeals to the dot
product, while the second solves a constrained optimization problem.

Let's calculate the ascent of a vector `v`. This is equal to:

    grad(f(a)) \cdot v

We know from linear algebra this equals:

    |grad(f(a))| |v| cos(\theta)

Where `\theta` is the angle between the gradient and `v`. So this is
maximized when `\theta=0`, which is when `v=grad(f(a))`.

Note: I lacked intuition for why the ascent is equal to `grad(f(a))
\cdot v`. This is obviously algebraically. But what is the geometric
intuition?

This is because we are reparameterizing the description of the
plane. Consider any vector; this can be decomposed into the gradient,
plus a part that is perpindicular to the gradient. But note that any
perpindicular vector cannot move up the plane! I am too lazy to prove
this right now. So we may disregard this.

TODO: I should show how to prove this using Lagrange multipliers.

TODO: I need to explain why the gradient is a natural step choice. I
presume it is because you want to make a small step in which you think
the plane is the best approximation.

TODO: Maybe I should run some experiments and see how fast we get
convergence for a quadratic bowl. I could compare gradient descent to
my randomimzed approach.

## TODO

* Newton's Method
* Conjugate gradient
* Line search
* Hessian Free Optimization?
