Consider sinusoidal functions of a fixed period `2pi / omega`. This is
the set:

    { a * sin(phi + omega * t) | a, phi in R }

Note that I don't need to include phase-shifted or amplitude-stretched
versions of `cos` as this is itself a phase-shifted version of `sin`.

In [another file](./space-of-sinusoidal-functions.md) I showed that
this space has as a basis `{ sin(t), cos(t) }`: that is, that any
function in the space can be written as a linear combination of these
*two* basis vectors. It is natural that the space has dimensionality
2, because all members of the space were parameterized by a tuple `(a,
phi)` as presented above.

Now, I would like to decompose any member `f` in the "original"
representation of `(amplitude stretch, phase shift)` to `(a, b)`,
where `a` and `b` are the amplitudes of `sin` and `cos` needed to sum
out to `f`. That other file actually already tells us how to perform
this calculation through an identity (because we showed we showed `{
sin, cos }` was a basis by showing *how* to decompose any member `f`).

Okay! That was too easy! Let us now say that we may measure `f(t)` at
any proposed time `t`, and that we know `f` has appropriate period,
*but* we do not know the "original" representation of `f`.

Now, I believe one iterative method would be to find a zero fo `f`. We
know exactly one zero exists for `t in
[0, pi / omega)`, and that if all values for `t in [0, phi)` are
negative then all values for `t in (phi, pi / omega)` are necessarily
positive. So searching should be simple.

Having found `phi`, we now can compute `a` by measuring
`phi + (2pi / omega) / 4`.

I want to do a way that generalizes to periodic functions of *unknown*
periods, so I will adopt another approach. Here is the idea. I want a
function `I(f, g)` where:

    I(f, sin) = a
    I(f, cos) = b

for exactly the correct `(a, b)` in the decomposition. Moreover, I
want to find a function `I` that I can calculate using only amplitudes
of `f` calculated at specific time.

Note that I don't care what `I(f, g)` is for `g` not in `{ sin, cos
}`. There are an infinite (indeed uncountable) number of ways to
extend `I`.

Note that `I` must be linear in the first argument when the second
argument is `sin` or `cos`. In fact, `I` will work as a decomposition
method whenever we have a basis such that (0) `{ e_i }` spans the
space, (1) `I(e_i, e_j)` is one exactly when `i = j`, and (2) `I(v,
e_i)` is linear in `v`.

If we make further requirements require that (1) `I` is *always*
linear in the first argument and (2) `I` is *symmetric*, then there is
actually only one possible `I`, as shown in my Strang notes. It will
be natural if these properties *do* hold, but I actually don't need
them for what I use `I` for. Which is all a long way of saying: I
don't need `I` to be a true inner product, but it probably will be
anyway.

I propose `I(f, g) = \Int_{-T/2}^{T/2} f(t)g(t)`. This feels like a
natural generaltization fo the dot product. In order to show this
works, it will suffice to show that (1) `I` is linear in the first
argument (at least when `g \in {sin, cos}`) AND that (2a) `I(sin, sin)
= 1.0`, (2b) `I(sin, cos) = 0.0` and (2c) `I(cos, cos) =
1.0`. Symmetry is clear, but I don't really need that, except that
(2b) implies its flipped version which I also need.

First, note that `I(af, g) = aI(f, g)` obviously. Then, consider `I(f
+ g, h)`. This integral can be written as `I(f, h) + I(g, h)` if those
two integrals exist and are not infinite. Now, the integral of two
bounded functions over a finite range must be finite if it exists at
all. Moreover, integral of two continuous functions over an interval
must exist. I don't prove those general statements, but this completes
the demonstration that `I` is linear in the first argument (and also
the second by symmetry, though I don't need that).

So I need only show that `I` does the right things for when `f, g \in
{sin, cos}`. Consider `I(sin, cos)`; `cos` is symmetric around `0`
while `sin` is antisymmetric. Thus, the integrating on `-T/2, 0` gives
opposite volume vs integraint `0, T/2`. That means that `sin` and
`cos` are orthogonal wrt `I`.

To show this, I will make a pairing argument. Note that because
circle, `sin**2 + cos**2 = 1`. Now, consider this: `cos` on the range
`[0, pi/2]` is the same as `sin` on the range `[pi/2, pi]`; likewise
for `cos**2` and `sin**2`. Thus, the integral of `sin**2` on `[0, pi]`
can be broken into integrals on `[0, pi/2]` and `[pi/2, pi]`, but then
the second is the integral of `cos**2` on `[0, pi/2]`. Which means the
sum is the same as integrating `sin**2 + cos**2` on `[0, pi/2]` which
is simply `pi/2`.

The same argument works on the left side. So the integral across the
period is `pi`. This suggests that we should in fact correct `I` by a
factor of `T/2`. But otherwise this works correctly. So what I really
want is:

    I(f, g) = T/2 \Int_{-T}^{T} f(t)g(t) dt

I believe this math is confirmed here: http://math.mit.edu/~gs/cse/websections/cse41.pdf

## Extending our result

So I have shown how to decompose all functions which are linear
combinations of `sin` and `cos` for a fixed period `T`.

What about *all* functions of period `T`? Surely this contains many
many bad actors, so let us restrict ourselves to functions integrable
on their period.

We might try to make life simpler by further restricting to functions
continuous on their period. But that would deny the square wave
function. So maybe we can restrict ourselves to *piecewise*
continuous, bounded functions. (Actually, piecewise continuity implies
boundedness on a closed interval). Our results will probably hold for
an even greater number of functions.

Are there such functions that cannot be written in terms of our
existing basis? The answer is yes; we already have as an identity that
any linear combination of `sin` and `cos` must also be a sinusoidal
function. But of course there are other functions of period `T` that
are *not* sinusoidal.

This space of picewise continuous functions cannot be spanned by a
finite basis. This is simple to see. Note that for any given `n`, the
set of indicator functions on `(k/n, k+1/n)` is linearly
independent. That means there isn't a basis of size less than `n`, but
since that is true for all `n`, there is no finite basis.

So we want an infinite basis then. In that case, we need to talk about
what it would mean for an infinite sum of basis vectors to converge to
a value. There are many notions of convergence in function spaces, but
we can adopt L2 convergence, which seems reasonable. The L2 norm is
square-root of the integral of the square of the function. It
generalizes the finite Euclidean norm, I suppose.

In that case, I propose a countable spanning set. It is the countable
union of indicator functions with period `T`. This is not an
independent set, but I don't care. It does span the space of periodic
functions. This will show the way to finding a true basis: if a
linearly independent set spans all periodic indicator functions with
rational endpoints, then that set is itself a basis.

I want to make a note about the space we are considering. We are
considering not just functionsn with period exactly `T`, but also
those functions whose period *divides* `T`. It would be fruitless to
try to limit ourselves to functions with period exactly `T`, because
this set is not closed under linear combination. For instance,
consider any member `f` and a second member `1 - f`. So these sum to
the constant function, which has period zero! I can show less
degenerate cases, but I hope this is satisfactory.

**TODO**: I think you can prove this by showing that the indicator
function on `(-x, x)` is always representable. That's even more direct
than showing for any indicator function with rational endpoints.

**TODO**: Show that `sin(nk)` does this!
