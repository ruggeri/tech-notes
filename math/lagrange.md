## Optimization Problem

Given a function and (possibly) some constraints, you want to find the
`x` such that `f(x)` is maximized.

## Unconstrained Optimization of Differentiable, Univariate Functions

Assume that `f` is differentiable; this is not a strong
assumption. (Later addendum: yes it is a fucking strong assumption).

If `f` is of one variable, then a local maximum is acheived if:
`f'(x)=0` and `f''(x)` is negative. If `f''(x)` is positive, this is a
local minimum.

If `f''(x)` is zero, then we must look to further derivatives. I don't
study these at this moment.

## Unconstrained Optimization of Differentiable, Multivariate Functions

Say that `f:\R^n\to\r` is differentiable; note that this is *stronger*
than the assumption that all partials exist. Take for instance:

    f(x, y) = { x if y != x, else 0 }

Partials exist wrt to `x` and `y` at `(0, 0)`, but *not*
differentiable. In the case that `f` is differentiable, its
differential is the hyperplane defined by linear combinations of the
partials.

We must talk about the second derivative test; some partials may be
positive, others negative, others zero. What we need is that the
Hessian be negative . That means that `H \cdot \delta{x}` is always `<
0`, so that the slope is always negative in the neighborhood, so that
a maximum is attained at `x*`.

(TODO: Reviewing this document, I am a little confused. I think all
partials must be zero, and all Hessian entries be negative.)

## Equality Constraints

Now say that you need to optimize a function wrt an equality
constraint. That is, say that you are also given a function `g(x)` and
required that `g(x)=0`: now optimize `f(x)`.

We will again assume that these are *smooth* functions (that all their
derivatives exist). That is a mathematically strong assumption, but
pretty realistic in many real-world problems.

For elucidation: assume `f` maps coordinates in a national park to
altitudes, while `g(x)=0` defines a path through the national
park. You want to find the point at which you reach the maximum
altitude on the path.

If `g` truly defines a path, then you could equivalently define this
path by `h(t)` such that `t\in\R` is a continuous mapping to points on
the path. You could then differentiate `f(h(t))` using the chain
rule. This should give you the maximum.

Finding this reparameterization of `g` could be hard. Also, this
approach would have difficulty generalizing if `g(x)=0` defined a more
complicated set than a path.

## Method of Lagrange Multipliers

Let us approach the problem differently. We know that we need an `x`
where the isoquant of `f` is parallel to `g`. If that were not true,
we could move a little along `g` (in one direction or another) and
move either up or down `f`.

Isoquants of `f` are perpendicular to `\grad f`. Likewise, the set
`g(x)=0` defines a manifold perpendicular to `\grad g`. So we know
that `\grad f` and `\grad g` must lie in the same subspace. If they
did not, we can project `\grad f` onto the subspace perpendicular to
`\grad g` and move a little along the manifold to pick up some
positive `f` change.

Therefore, we have `\grad f = \lambda \grad g`. The constant `\lambda`
is called the *Lagrange multiplier*. At most points `x`, there is no
such `\lambda`, but at a minimum or maximum there must be such a
`\lambda`.

Here is how I would solve the problem. I would set `\grad f = \lambda
\grad g`. I would then solve for `x` in terms of `\lambda`. I then
plug this formula back into `g(x) = 0`. Even if `g` originally
consisted of many variables, the substitution makes it an equation in
a single variable: `\lambda`. Therefore I can solve it.

Having found the appropriate `\lambda`, I then use my formula to
convert it back to `x`.

## Example

Let's say we want to maximize `f(x, y) = x + y`. We are subject to the
constraint `g(x, y) = x^2 + y^2 - 1 = 0`. Basically: find the point that
maximizes `x + y` on the unit circle.

I then do like so:

    \grad f = \lambda \grad g
    (1, 1) = \lambda (2x, 2y)
    (1/[2 \lambda], 1/[2 \lambda]) = (x, y)

Now I ask: why may I assume that `\lambda != 0`. Because if `\lambda =
0`, that means I wouldn't pollute even if you didn't charge me. So in
that case an optimum for `\lambda = 0` would be an optimum for any
`\lambda != 0`.

Having solved for lambda, I focus on the constraint:

    g(x, y) = x^2 + y^2 - 1 = 0
    2 (1/[2 \lambda])^2 = 1
    1/2 lambda^-2 = 1
    lambda = 1/sqrt(2)

This then implies:

    (x, y) = (\sqrt(2) / 2, \sqrt(2) / 2)

## DJ Shadow Price

The lambda value is often called the *shadow price*. Here is how I
interpret it:

The function `f(x)` is the dollar value of the widgets your firm
produces for a given setting of the knobs `x`. However, you will also
produce `g(x)` is the dollar value of the pollution your firm will
create in doing so.

The laws of your enlighted country prohibit you from polluting. That
`g(x)` is constrained to zero.

However, some smart-ass suggests that it would be better to create a
market for pollution credits, with the government selling them at a
price `\lambda`. The question is: what price should the government
set?

In the pollution credit regime, your firm wants to maximize:

    f(x) - \lambda g(x)

That is, it wants `\grad f(x) - \lambda \grad g(x) = 0`. This is the
case at any price `\lambda`. However, at some prices `\lambda` you will
choose to pollute. The government wants to enforce `g(x) = 0`, which is
an additional constraint.

One way to express this constraint is that the partial of `\lambda
g(x)` wrt `\lambda` is zero. That is really just a stupid restatement
of `g(x) = 0`; it says nothing new. This does admit a new
interpretation, though: that your firm is insensitive to small
adjustments in the price of pollution credits: because you use none!

Therefore, we may say that the solution to the constrained problem,
which is really a team-effort of you and the government is, to
maximize

    f(x) - \lambda g(x)

wrt both `x` and `\lambda`. Any local maximum must satisfy: (1) you
don't pollute at all, else the government could help your firm by
lowering the pollution price and (2) there is no way to increase
production without also increasing pollution.

## Multiple Equality Constraints

Just add more shadow prices!

## Inequality Constraints

Now let's say we want `g(x) <= 0`. In this regime, you are not allowed
to pollute still, but you *are* allowed to have a net positive effect
on the environment and clean things up if you like.

Let's consider

    f(x) - \lambda g(x)

again. Now things aren't quite as simple as just maximizing this
quantity. Instead, we want to note a number of conditions of an
optimal `x`.

For any optimal `x`, there exists a shadow price `\lambda` where:

(1) f(x) - \lambda g(x) is maximized wrt `x`; that is the partial wrt
    `x` is zero,
(2) \lambda >= 0. That is, you are never *paid* to pollute.
(3) \lambda g(x) = 0. That is either: (a) even if the price of pollution
    credits is lambda, then you wouldn't be motivated to do any cleanup,
    OR (b) you would do cleanup even if you weren't paid anything to do
    so, that is, lambda = 0.
(4) Of course, g(x) <= 0.

So what am I saying here? I'm saying that your behavior with the
inequality constraint corresponds to what your behavior would be for
some shadow price. If your optimal behavior under the constraint is to
do some cleanup, then that is compatible with the shadow price `lambda
= 0`. Alternatively, if the optimal behavior is to do no cleanup, then
that means that there is some `lambda > 0` where this is exactly the
right thing to do at that price.

## KKT Example

Follows from http://www.math.ubc.ca/~israel/m340/kkt2.pdf.

Let's say our requirements are:

    maximize f(x, y) = xy
    x + y^2 <= 2
    x >= 0
    y >= 0

I threw some `>=` for fun!

Let's encode this with prices like so. `\lambda_1` encodes the price
for pollution `x + y^2`. `\lambda_2` is the credit for producing
positive byproduct `x`, `\lambda_3` is the credit for producing
positive byproduct `y`.

At prices `\lambda_1, \lambda_2, \lambda_3` we know:

    y - \lambda_1 + \lambda_2 = 0 (from deriv wrt x)
    x - \lambda_1 (2y) + \lambda_3 = 0 (from deriv wrt y)

Our *complementary slackness* conditions say:

    \lambda_1 (2 - x - y^2) = 0
    \lambda_2 x = 0
    \lambda_3 y = 0

This says that at an optimal solution to the program, you would act as
if

    (1) the price for polution \lambda_1!=0 entailed no polution, OR
        you wouldn't polute even if \lambda_1 = 0,
    (2) you would do x cleanup even if lambda_2 = 0, OR you wouldn't
        pollute if \lambda_2 were set appropriately,
    (2) you would do y cleanup even if lambda_3 = 0, OR you wouldn't
        pollute if \lambda_3 were set appropriately,

So let's begin! Let's briefly assume that behave optimally even if
\lambda_1 = 0. By our `x, y` optimality equation, that means that:

    y = -\lambda_2
    x = -\lambda_3

And by complementary slackness, we must have `x = y = 0` and
`\lambda_2 = \lambda_3 = 0`. This doesn't sound like it will be a
maximum of `f`, but maybe. Or maybe we derived this solution from a
false premise that `\lambda_1 = 0`.

Alternatively, we consider if `\lambda_1 != 0`. In that case we have
some subquestions. What if `\lambda_2 = 0`? Then:

    y = \lambda_1

from our maximization constraint. Since `y != 0`, that means that
`\lambda_3` doesn't matter, so we may assume `\lambda_3 = 0`. This is
convenient because it lets us enumerate fewer cases; we don't need to
solve for if  `\lambda_3 != 0`.

Using `\lambda_3 = 0`, we use our other optimization constraint
equation:

    x - \lambda_1 (2y) = 0
    x - 2 \lambda_1^2 = 0
    x = 2 \lambda_1^2

Last, we can solve for `\lambda_1`.

    2 - (2\lambda_1^2) - (\lambda_1^2) = 0
    2 = 3 \lambda_1^2
    \lambda_1 = \sqrt(2/3)

Thus:

    y = \sqrt(2/3)
    x = 4/3

There is one last case! We must check for when `\lambda_1 != 0` AND
`\lambda_2 != 0`. `\lambda_2 != 0` implies that `x = 0`. But then
`\lambda_1 != 0` implies `x + y^2 = 2`, so we have `y = \sqrt(2)`.

So in the final analysis, we examine the possibilities:

(1) x = y = 0,
(2) x = 0, y = sqrt(2)
(3) x = 4/3, y = \sqrt(2/3)

Clearly 3 is the maximizer.
