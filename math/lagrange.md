## Optimization Problem

Given a function and (possibly) some constraints, you want to find the
`x` such that `f(x)` is maximized.

## Unconstrained Optimization of Differentiable, Univariate Functions

Assume that `f` is differentiable; this is not a strong assumption.

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
positive, others negative, others zero. **TODO2**: Generalize for
multiple dimensions.

## Equality Constraints

Now say that you need to optimize a function wrt an equality
constraint. That is, say that you are also given a function `g(x)` and
required that `g(x)=0`: now optimize `f(x)`.

We will again assume that these are *smooth* functions (that all their
derivatives exist). That is a mathematically strong assumption, but
pretty realistic in most real-world problems.

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

## Lagrange Multipliers

Let us approach the problem differently. We know that we need an `x`
where the isoquant of `f` to be parallel to `g`. If that were not
true, we could move a little along `g` (in one direction or another)
and move either up or down `f`.

Isoquants of `f` are perpindicular to `\grad f`. Likewise, the set
`g(x)=0` defines a manifold perpindicular to `\grad g`. So we know
that `\grad f` and `\grad g` must lie in the same subspace. If they
did not, we can project `\grad f` onto the subspace perpindicular to
`\grad g` and move a little along the manifold to pick up some
positive `f` change.

Therefore, we have `\grad f = \lambda \grad g`. The constant `\lambda`
is called the *Lagrange multiplier*. It is trivial to find `\lambda`
for a given maximum `x`, but in general we do not know it without
knowing `x`.

Let us use this to reformulate the constrained problem *as an
unconstrained problem*. We introduce a new function `L` called the
Lagrangian:

    L(x, lambda) := f(x) + \lambda .* g(x)

We now want to find `(x*, \lambda)` where

    \grad L = 0

Since the partial with respect to `x` is zero, we know that isoquant
defined by `f(x)=f(x*)` is parallel to `g(x)=g(x*)`. Note also that we
require the partial wrt to `\lambda` to be zero. This is ensuring that
`g(x*)=0`!

## Further Notes

* We should generalize to multiple equality constraints.
* We should generalize to inequality constraints (KKT).
