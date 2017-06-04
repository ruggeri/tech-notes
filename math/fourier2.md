## Basis for sinusoidal functions of period T

Consider sinusoidal functions of a fixed period `T`. This is
the set:

    { a * sin(phi + *(2pi/T)t) | a, phi in R }

Note that I don't need to include phase-shifted or amplitude-stretched
versions of `cos` as this is itself a phase-shifted version of `sin`.

In [another file](./space-of-sinusoidal-functions.md) I showed that
this space has as a basis `{ sin((2pi/T) t), cos((2pi/T) t) }`: that
is, that any function in the space can be written as a linear
combination of these *two* basis vectors. It is natural that the space
has dimensionality 2, because all members of the space were
parameterized by a tuple `(a, phi)` as presented above.

## Inner Product and Finite Approximation

Now, I would like to decompose any member `f` in the "original"
representation of `(amplitude stretch, phase shift)` to `(a, b)`,
where `a` and `b` are the amplitudes of `sin` and `cos` needed to sum
out to `f`. That other file actually already tells us how to perform
this calculation through an identity (because we showed we showed `{
sin, cos }` was a basis by showing *how* to decompose any member `f`).

**An Iterative Method By Sampling**

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

**Toward an inner-product**

I want to do a way that will eventually generalize to periodic
functions of *unknown* periods. In that case, the above method would
not work, so I will adopt another approach.

If I have an inner product on the space that declares `sin` and `cos`
to be orthogonal unit vectors, I would be done, per Strang algebra
notes. I will introduce one. The idea is that I will want to be able
to approximate projections of `f` onto the basis even without access
to a closed form for `f`.

**Aside: I only care about projection onto my basis**

Btw, I don't need a full and true inner product. I just need `sin` and
`cos` to be orthogonal unit vectors, and for the pseudo-inner product
to be linear in the first argument when the second is either `sin` or
`cos`. At that point, I can prove that this function decomposes any
`f` into a sum of `sin` and `cos` properly. This is an elementary
linear algebra fact. I mention this to set expectations
correspondingly low.

Note that all functions that properly decompose `f` into `sin` and
`cos` are identical when the second argument is `sin` or `cos`; they
have to be, since `sin` and `cos` are linearly independent and thus
there is a unique decomposition of `f`. Of course, there are an
infinite number of functions that do this. As mentioned in my Strang
notes, each function may "privilege" different bases `B` by allowing
you to decompose `f` properly into a linear combination of `B`. We saw
that dot-products privilege rotations of the reference basis.

Note that if we require that a candidate (1) is *always* linear in the
first argument and (2) is *symmetric*, then the decomposition function
*must* be unique. That's because for `<v, w>` you decompose `v` into a
linear combination of basis vectors, which means the inner product is
a linear combination of `<e_i, w>`. But then you use symmetry to flip
this around, and now all your inner products are on the original
locked down domain. So you have no choice for `<v, w>` at that point.

**A proposed inner product**

I propose `I(f, g) = \Int_{-T/2}^{T/2} f(t)g(t)`. This in some ways
feels like a natural generalization of the dot product, though I hope
to provide more motivation soon (when I figure it out!).

In order to show this works, it will suffice to show that (1) `I` is
linear in the first argument (at least when `g \in {sin, cos}`) AND
that (2a) `I(sin, sin) = 1.0`, (2b) `I(sin, cos) = 0.0` and (2c)
`I(cos, cos) = 1.0`. Note that because symmetry is clear, 2b will
imply `I(cos, sin) = 0.0`. But I don't really need to prove symmetry,
to be honest, if I were to show this extra fact.

**I always exists**

Right now, we're talking about a space of sinusoidal functions, which
are continuous and thus integrable on a closed interval. The product
of two sinusoidal functions also has finite integral on a closed
interval. In fact, this is true for any piecewise continuous function
on a closed set. So we needn't fear whether integrals exist at the
moment.

I'm not going to prove those statements, because I am not a walking
calculus textbook.

**Linearity**

It is clear that `I(af, g) = aI(f, g)`.

Next , consider `I(f + g, h)`. This integral can be written as `I(f,
h) + I(g, h)` since those two integrals exist and are neither is
infinite.

**Sin and Cos are declared orthogonal unit vectors**

So I need only show that `I` does the right things for when `f, g \in
{sin, cos}`. Consider `I(sin, cos)`; `cos` is symmetric around `0`
while `sin` is antisymmetric. Thus, the integrating on `-T/2, 0` gives
opposite volume vs integraint `0, T/2`. That means that `sin` and
`cos` are orthogonal wrt `I`.

Next I must show `I(sin, sin) = 1`. To show this, I will make a pairing
argument. Note that because circle, `sin**2 + cos**2 = 1`. Now, consider
this: `cos` on the range `[0, pi/2]` is the same as `sin` on the range
`[pi/2, pi]`; likewise for `cos**2` and `sin**2`. Thus, the integral of
`sin**2` on `[0, pi]` can be broken into integrals on `[0, pi/2]` and
`[pi/2, pi]`, but then the second is equal to the integral of `cos**2`
on `[0, pi/2]`. Which means the sum is the same as integrating
`sin**2 + cos**2` on `[0, pi/2]` which is simply `pi/2`.

The same argument works on the left side. So the integral across the
period is `pi`. This suggests that we should in fact correct `I` by a
factor of `2/T`. But otherwise this works correctly. So what I really
want is:

    I(f, g) = 2/T \Int_{-T}^{T} f(t)g(t) dt

I believe this math is confirmed in the Strang chapter linked here:
http://math.mit.edu/~gs/cse/websections/cse41.pdf

## Inner Product Explored for all Piecewise Continuous Periodic Fns

**Periodic Piecewise Continuous Functions**

So I have shown how to decompose all functions which are linear
combinations of `sin` and `cos` for a fixed period `T`.

What about *all* functions of period `T`? Can those be decomposed into
linear combinations of `sin` and `cos`? Surely this set contains many
many bad actors, so let us restrict ourselves to functions integrable
on their period.

We might try to make life simpler by further restricting to functions
continuous on their period. But that would deny the square wave
function. So maybe we can restrict ourselves to *piecewise* continuous
functions. These functions are by necessity both bounded and
integrable.

**Sin and Cos Do Not Span**

Are there such periodic piecewise continuous functions that cannot be
written in terms of our existing basis? The answer is yes; we already
have as an identity that any linear combination of `sin` and `cos`
must also be a sinusoidal function. But of course there are other
functions of period `T` that are *not* sinusoidal: any constant
function is an example.

Please note: we have opened ourselves up to functions also periodic on
lengths less than `T`, so long as their period divides `T`. We would
have mighty difficulty keeping these functions out even if we wanted
to consider only functions with period exactly `T` but no less. For
consider an indicator function which is zero on `[0, T/2)` and one on
`[T/2, T]`; if you add this with a corresponding reversed function,
you'd get the constant function `1`, which has period zero. In any
case, we should welcome the diversity of the space of functions we are
considering! :-)

**Periodic Piecewise Continuous Functions Have Inifinite Dimension**

This space of piecewise continuous functions cannot be spanned by a
finite basis. This is simple to see. Note that for any given `n`, the
set of indicator functions on `(k/n, k+1/n)` is linearly
independent.

It is a theorem of linear algebra that if a space has dimension `n`,
then there exists no linearly independent set of size `>n`. But since
there in fact *does* exist sets of independet periodic
piecewise-continuous functions of length `>n` for *every* `n`, this
shows there is *no* finite basis for the space.

**Where we're going, we need topology**

So we know that only a set of infinite size could hope to span this
space. This opens up the possibility of linear combinations of basis
vectors where an infinite number of `\alpha_i` are non-zero. That
implies taking an infinite sum, which means we will need a topology on
the space.

Before we introduce a topology on this space, let's ask if we really
need it? Could it be possible that we need a basis of infinite size,
but could just use finite sums? A basis where the elements of the
vector space are all finite linear combinations is sometimes called a
*Hamel basis* in honor of the actor from Star Wars.

It appears typical of infinite dimensional spaces that, in order for a
Hamel basis to span the space, it must be of uncountable size, which
sounds too big to be very useful. In particular, we could not
itervatively project onto each basis element and hope to produce a
decomposition that converges to the vector.

Indeed, I'm pretty sure no countable set can be a Hamel basis. Note
that the periodic functions of period `T` include every function that
indicates a `nT + alpha`, where `alpha` is a real number. This is an
uncountable set of linearly independent functions. So a Hamel basis
would be necessarily uncountable, if I am reasoning correctly.

So we see that if we want a countable basis, we need a topology. If we
have a topology, would we still need a countable basis? Could we take
an infinite sum of a finite set of basis vectors and that would now
span? No: an infinite sum of a finite basis set is equivalent to some
finite sum, so we don't get more power that way.

The point is: we're going to need to expand our world to a countable
basis, and in order for that to be good enough, we'll need a notion of
convergence of infinite sum.

**Topological Tangents**

So our goal is a *countable* basis. Since we saw that no countable
basis can span in terms of finite sums, we know that if we are to have
a countable basis it needs to be in terms of *infinite* sums. Infinite
sums are about convergence. That means we need to introduce a
topology: we want a *topological vector space*.

We know there are many definitions of convergence for functions. So we
may ask: what would be a good choice for us? One thought I had was
that we'd like the space to be *complete*: we'd like every series of
functions to converge to a member of the space.

But wait. Not every sequence of *real* numbers converges! Does that
mean that the reals are not complete?? Answer: completeness is
typically defined in the setting of a *metric space*. When you have a
metric, you can have a notion of *Cauchy sequence*, which means that
members eventually all lie within a distance `epsilon` of each other,
for arbitrarily small epsilon. A complete metric space has every
Cauchy sequence converge to a member. So sequences like `2**i` are
*not* Cauchy, and *don't* need to converge.

Can we have a notion of completeness for a topological space? Answer:
kinda. Sometimes mathematicians talk about a *metrizable topological
space*: one where the topology can be induced by a metric. In that
case, this is sometimes called a *topologically complete space* or a
*completely metrizable space*.

Maybe a more basic question: do we even need a topology? Could we just
specify a notion of convergence, without that notion specifying what
are open sets? For instance, what topology corresponds to pointwise
convergence? Answer: it sounds like the *product topology*. It would
be nice to show how you can always recover a notion of topology from a
notion of convergence, but perhaps this is very hard, because multiple
topologies may be compatible with the same notion of
convergence.

**Integral inner product allows approximations**

I've gotten a little distracted by theory. Let's circle back and think
about how we plan to break up a function `f` into a linear combination
of basis vectors. Basically: how do we plan to do the projections?

The space of piecewise continuous functions is extremely
varied. Whatever process of projection is going to have to work for
many antagonistic vectors to project; it would be incredible if this
were always simple.

It would sure be nice if we could take a finite approximation of `f`
and a finite approximation of a basis vector `e_i`, and perform some
kind of projection of the `f` approximations onto the `e_i`
approximation. Then, hopefully, in the limit, this would converge to
the proper approximation.

Of course, the simplest approximation is to break `f` and `e_i` into
step functions. That effectively makes them vectors in `R^n`. The
simplest inner product here is just the dot product. And of course, in
the limit, that will converge to the integral of `f` times `e_i`. That
suggests we use this as our inner product.

I don't see that we're forced to use this inner product. But it seems
like any other inner product would just be making our lives
difficult. It may even be that all other inner products are in some
sense this integral product, but I don't know that. At the moment, it
seems perverse to consider alternatives, so I will not.

**From inner product to norm and topology**

We've decided how we want to decompose a function. But in what sense
is the "recomposition" the original vector? In a finite dimensional
space, after decompsition, we could restore and get exactly the
original vector. But here, the "restoration" is actually a sequence of
restorations. In what sense does this converge to the original?

Consider a function `f` and a finite decomposition and recomposition
`f_i`. The "error" is `f - f_i`. If we had `f - f_i` in the basis,
then adding in exactly one times `f - f_i` would give us exactly `f`.

Let's consider `I(f - f_i, f - f_i)`. If `f_i = f`, it makes sense
that this is zero; though I suppose it could be anything, because
adding zero to the basis makes a non-linearly independent set.

On the other hand, when `I(f - f_i, f - f_i)` is non-zero, this means:
"I wish I had some of the error vector `f - f_i`."

On the third hand, consider the possibility that `f != f'` but `I(f,
f - f') = 0`. That means: "`f` and `f - f_i` are not equal, but only
in a sense that I don't care about. Even if you gave me the exact
'fixup' vector, I would choose not to use it."

Let us say that a sequence `f_i` converges to `f` if the error's
*norm* has limit zero (that's what we call: "how much do you want to
use of yourself to reconstruct yourself?). It can be shown that thea
reconstitution error will, in the limit, always be orthogonal to all
basis vectors. Therefore, the reconstitution always converges to `f`
if the basis spans, *or* to a vector `f\hat` with non-zero norm whose
error is orthogonal to all basis vectors.

**L2 convergence is a stronger notion than pointwise convergence**

Consider a series of functions `f_i` which take the value `3**i` on
the interval `1/2**i - 1, 2**(i+1) - 1`. This converges pointwise to
the zero function (which has integral zero) but the norm of the L2
error diverges.

No matter the basis, we would never see an approximation series like
this (provided we use an orthogonal basis). The approximation will
converge to some `f\hat`, which may not be the true `f`, but it won't
*diverge*!

Moreover: A basic result is that if `f` is in the span of an
orthogonal basis, the sequence of absolute areas of each approximation
`f_i` will converge to the absolute area of the original `f`.

This basically says: if you care about the average power of a signal,
the approximations will not lie to you!

It is *not* true that for any `F`, that `lim F(f_i) = F(f)`. This
cannot be so for every `F`. However, hopefully for many common and
useful applications, we will be able to use the basis transformation
like that. Indeed: it is the whole point of this exercise!

**Stronger notions of convergence?**

When you say that it isn't safe to assume `lim F(f_i) = F(f)`, that
makes me wonder whether we can find a decomposition method such that,
when we decompose a function, the convergence is stronger, and thus we
`lim F(f_i)` will converge to the correct value for a greater class of
`F`.

This is a question beyond my pay grade. I have worries. The first is
that your decomposition method may need to be very complex. Is there a
method of finite approximation? My second worry is that your basis may
have to be far too large. In particular, if the basis is of
uncountable dimension, is it possible that a series of *finite*
approximations will converge (in your stronger sense!) to the target
`f`?

## Arriving at the Fourier Basis

By now I have thoroughly convinced you that the integral inner product
is the most common-sense way to perform projection in a space of
piecewise continuous functions on a domain of finite volume.

But now we must ask: what basis should we use? Obviously any
orthogonal set that spans the space will work. What do those look
like? What are our choices?

**Beginning To Produce A Basis**

I propose a countable spanning set. Consider all indicator functions
of open sets with rational endpoints. This is countable. There is
clearly a pointwise convergent series of finite linear combinations of
this basis for any pointwise continuous fuction `f`. I will not show
that this converges in L2 norm, because I am lazy.

This set is not independent, so there is certainly not a unique
decomposition into these functions. What about indicator functions
`[0, q)` for rational `q`? Also not independent, in the sense that you
can build a series of "staircases" that converges to the constant
function `[0,1)` (start with 1/2 of `I_{[0,1)}` and 1/4 of `I{[0,
1/2)}`.

**Why do we need an orthogonal basis**

Duh. If the basis is not orthogonal, then we can project onto the
basis vectors and get a proper deconstruction. When you reconstruct
you're not going to get the original vector!

On the other hand, maybe we can introduce a series of finite, bases
that each consist of orthogonal vectors (their countable union does
*not* consist entirely of orthogonal vectors), each yielding a more
accurate deconstruction of `f`. Of course, we can do this with a
series of step-function approximations.

But now we have to remember the point of what we're doing at all. We
*know* we can approximate a function by measuring it at points `i/k`
and constructing a step function. That's *easy*. But if we presume our
function is in fact the sum of sinusoidal functions AND we'd like to
do different things to those subfunctions based on their frequency,
then the step-function approach is not helpful. Indeed, if we want to
do something like a bass cut or a treble cut we specifically want to
operate on certain frequency.

In particular, you might want to ignore "high-frequency effects" and
say your approximation is plenty good if it captures the low-frequency
effects. For instance, if you are a trader, maybe effects that are
high frequency oscilate too quickly for you to take advantage of.

Indeed, the point is this. You want an approximation in a domain where
the calculations you want to make are *easy to perform.*

## TODO

1. The next natural step is to show you can approximate steps with
sinusoidals, which will show that they span the space. It's probabily
easiest for `I_{-x, x}`.

2. We know there are other orthogonal bases other than sinusoidals (at
least for finite dimensional spaces). Are these all in some "sense"
generalizations of the sinusoidal case? I doubt so! For instance, the
wavelet transform!

3. We know we can *rotate* a basis and maintain orthogonality. For the
sinusoidal basis, this means phase shifts. Can you explain intuitively
why? But there are many other orthogonal bases: can you "rotate" to
them?

4. Are there any other natural basis choices other than sin/cos? We
know that for finite dimensions, it doesn't matter, they're all
rotations of each other. Is that true here?

5. Sin/cos seem like they wouldn't be a ntural choice. But maybe they
are: maybe sin/cos are the limit of a series of rotated bases?

6. Remember, if we are talking about a finite dim function space that
is spanned by sin/cos, then not every orthogonal basis of dimension
two will span the same space.

Some maybe useful links:

https://dsp.stackexchange.com/questions/17212/is-there-any-alternative-basis-for-a-fourier-like-transform
