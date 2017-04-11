## Univariate Model

So, consider a univariate function, where the second derivative is
constant and positive. This function has a global minimum.

Consider a point; we want to move to the minimum. The correct thing to
do is get the derivative, divide by the second derivative, and make
that your delta.

Let's say we don't know the second derivative. We can guess the second
derivative. Call this `f''*` (this is the inverse of the *learning
rate*). Then, if `f'' < f''*`, we will undershoot but converge. if
`f''/2 < f''* < f''`, we will overshoot, but the distance to the
minimum is not less than half what we thought, so we do get closer and
will converge. If `f''* < f''/2`, we will *diverge*.

## Independent Bivariate Model

Consider a 2D scenario where we update `x_1` and `x_2`
seperately. This is effectively what we do with gradient descent,
since no mixed partial information is used. In that case, we may want
to use different learning rates for `x_1` and `x_2`, since the second
partial wrt `x_1` may be much greater than the second partial wrt
`x_2`. This scenario describes a *ravine* with steep walls on the
`x_1` side and slowly sloping on the `x_2` dimension.

In the case we can only use a single global rate, we'll either
overshoot along the `x_1` dimension or undershoot along the `x_2`
dimension. Moreover, to avoid divergence in the `x_1` dimension, we
can only make the learning rate so high, and so there is a maximum
rate of progress along the `x_2` dimension.

## Convex Models: Good Cases

Let us say that `f` is convex; that `f''>0` everywhere. That of course
means that `f'` is monotonic. We are simply relaxing the assumption
that `f''` is constant. Because the function is convex, it continues
to have at most a single global minimum. (Note that a function like
`1/x` is convex on `(0, \inf)` but has no minimum.

Let us say that `f''` is bounded above. Then we can pick `f''*` such
that it is greater than `f''` everywhere, and thus always undershoots.

Now, if `f''/f''* > \eps` everywhere we will converge: each step we
move at least a fixed percentage toward the minimum. But the only way
to avoid this situation is if `f''` approaches zero. If we assumed
that `f''` is continuous, then this can only happen if there is no
global minimum at all.

Of course, we don't know `sup f''`, so we may pick a value below
it. Now, if the minimum lies in a region where `f'' > f''*` and our
guess starts in this region, we are guaranteed convergence.

Let us say that the minimum is in a region where `f'' > f''*`, but we
start outside this region, *but* in a region where `f''* >
f''/2`. Then our first step moves closer to the minimum. If we *stay*
after this first step in the region where `f''* > f''/2`, we know now
we will *always* stay in this good enough region. We will overshoot
for a while maybe, but always getting closer, and eventually we fall
in the region where `f'' > f''*`, and then we never overshoot and
steadily approach.

In fact, it wasn't necessary to assume that `f''> f''*` at the
minimum. So long as `f''* > f''/2` and our first step is okay, then
everything will be fine (though we'll keep overshooting, but
less-and-less).

Let us call such a region an *attractor* because any starting point
inside will be guaranteed to converge to the minimum.

## Tougher Cases

What if we start outside an attractor? Is it possible to be close
enough to an attractor such that you will still inevitably fall into
the attractor? Surely it is possible to diverge, if you jump over the
attractor, and then continue to jump further and further distances
over the attractor. Is that typical?

Note that `f''*` can be `>f''` at a point, but if the intervening
points are not also less than `f''*`, it is very possible to
overshoot.

Consider any region containing the minimum where `f''` is bounded
below. Then the overshoot ratio is also bounded. If the region is
large enough, even taking into account the overshoot, you will stay in
this region. You won't necessarily converge, though, but at least you
won't *diverge*.

I don't want to consider this anymore.

## Second Order Information

Our problem is that we assume the curvature is constant, but we don't
know the curvature. It's an extremely strong assumption, but we have
extremely limited information. We have to take a blind guess at the
curvature.

On the other hand, if we can measure the second derivative, and the
second derivative is constant, we can exactly solve the problem.

But, if we don't assume the second derivative is constant, we're back
in the same boat. We can try to make an assumption that the second
derivative doesn't change "too fast", which corresponds to some other
learning rate on the second derivative.

## Momentum

Momentum tries to deal with the ravine problem. The idea is this: in
dimensions in which the learning rate is too low, you will pick up
speed. In the dimensions in which the learning rate is too high, you
dampen this effect of the overstepping. However, if the learning rate
is much too high, you ought to be able to diverge still! However, that
requires that each overstep be twice the prior overstep!

If you undershoot, it's because the second derivative was not as steep
as you thought. Therefore, you should perhaps assume the second
derivative is less steep. That seems to me to suggest increasing the
learning rate appropriately. Perhaps you will say: "look, the
derivative changed by an observed amount in the prior step, so if we
divide by the length of the step, that suggests an average second
derivative." It may be too aggressive to set this learning rate using
this estimated second derivative, but maybe you can smooth this with a
decay constant?

I'm thinking: what if you make a step which undershoots by a tiny
amount? That is, your estimate of the second derivative seems good. If
you've picked up a ton of momentum, wouldn't you want to start hitting
the brakes?

## Adagrad

Adagrad adjusts the learning rate for a parameter to:

    \theta_{i, t+1} = \theta_{i, t} + (\alpha / G_{i, t})*g_{i, t}

With:

    G_{i, t} = \sqrt(\eps + \Sum_{t'<t} g_{i, t'}**2)

(The `\eps` is there to make sure this is never equal to zero).

Why do this? It seems like what they want to do is increase the
learning rate for features that are uncommon, versus for features that
occur frequently. If a feature is infrequent, presumably it has a
small gradient because it is zero?

This seems very differnt from what momentum was trying to
solve. Momentum was about your assumption of the change in curvature
being wrong. If you care about correcting that, you should be
computing what the curvature actually was, and using that for future
calculations.

For instance, the step size for Adagrad stays constant if the gradient
stays constant. But for me that suggests that you should *increase*
the step size! That's what momentum would do!
