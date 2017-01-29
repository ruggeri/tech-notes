Newton's method in optimization is an algorithm for finding a
stationary point. Note that this method is different than the better
known Newton's Method for finding roots of an equation (that is
sometimes also called Newton-Raphson).

The method basically assumes the surface can be approximated by a
quadratic function, and makes the appropriate move that would minimize
this quadratic function. This is covered in the quadratic optimization
document. The general idea is: take the derivative and divide it by
the second derivative; move this many units in reverse.

In the univariate case, if the function actually *is* quadratic, then
we solve for the stationary point in a single step.

In the univariate case, given some technical conditions, this process
will always converge to a stationary point.

## Multidimensional

We showed that even with skewed parabola axes, we could compute the
required movement. This required some equation solving, and I said to
myself that there is probably a simple algebraic encapsulation of the
ideas I used. Indeed there is!

Remember that the Hessian matrix is the matrix of second partial
derivatives. In the case of a parabolic surface, all second partials
are constant. Therefore, to calculate the change in the gradient given
a proposed movement `v`, we may calculate:

    Hv

Note that if `H` were not constant, we would have to calculate the
line integral from `x'` to `x''`. But I get off easy today, because
constant second partials!

Now. The change we wish to see in the world is exactly `-∇f(x')`. Our
question is what movement results in that change? This is easy! Simply
invert the Hessian and apply it to the `-∇f(x')`!

Wow. This is beautiful!

## Notes

First, the surface may not be parabolic. In that case we may want to
use a step-size less than one so we do not overshoot.

Inversion of the Hessian could be expensive in many
dimensions. Therefore, there are approaches that attempt to avoid
constructing either the Hessian or its inverse or both:

1. We don't really need the inverse; we just need to solve the system
   of linear equations: `H(x)v = -\grad(x)`. Apparently there are
   certain factorizations which will make this faster. Likewise, a
   solution can be approximated quickly to great accuracy.
2. quasi-Newton methods involve building up an approximation of the
   Hessian from changes in the gradient. BFGS is a quasi-Newton method.

## Further

Newton's Method for optimization is just a simple application of
Newton's Root-Finding Method to the gradient. They're really the same
method.

In fact, there's a great deal written about rates of convergence and
preconditions for Newton's Method on that wikipedia page, and I should
look that other soon!

## TODO

It is claimed that quasi-Newton methods are multidimensional
generalizations of the **secant method** for root finding. Thus it
probably makes sense to explore this next!

https://en.wikipedia.org/wiki/Secant_method
