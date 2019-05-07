**Functions of Finite L2 Norm Not Spanned**

Let me call attention to those functions `f` that have finite L2 norm.
For any `\omega`, we must assign that zero mass in the decomposition.
This is because the denominator of the "inner product" keeps increasing
when integrating over a greater `T`, while the numerator does not. Thus
the decomposition into `exp(i \omega t)` is identically zero. Which is
to say: no Fourier mass decomposition for `f` exists.

What can we resort to if we cannot assign a positive mass to any basis
vector? That is, what can we do if any finite or countable sum of basis
vectors involves simply adding zeros? We can move to integration: assign
mass to *sets* of basis vectors.

How to assign mass to sets? We could assign a *density* to every basis
vector, which tells you how to calculate the mass in neighborhoods of
that basis vector.

We can ask: what functions are decomposable into a Fourier
series/transform? More precisely: which ones will be reconstituted
properly when we invert the process of decomposition?

I will note that if `f` is "mass decomposable," then adding any finite
L2 norm adjustment will not change the mass decomposition. So the mass
decomposition can miss a lot of interesting phenomena. Again, we will
not converge in the `L2` norm, because of the finite norm adjustment.
But we may capture approaching 100% of the norm (minus that fixed finite
amount, which is 0%)...

**For Finite/Countable Mass Sums, Did We Need The Fourier Transform Over
The Reals?**

Question: did switching to integration over the real line give us new
power for mass decompositions that we didn't have before? If we knew
that `f` were a finite sum of sinusoidals, we could have computed its
Fourier series on a finite interval `T`, if we also knew the `T` to
use... Since we can never *observe* a function over the entire real
line, isn't our function `f` of our own "choosing?" Wouldn't in most
cases we be able to know `T` directly, then?

What about if `f` is a linear combination of *countably* many
sinusoidals? That could be aperiodic. Perhaps there is no finite `T`
that would work.

I want to note: we've really stretched the notion of "convergence" way
beyond the breaking point if the magnitudes of the linear combination
weights aren't required to converge to zero. Because if the weights keep
increasing, then that means we are never really approaching "capturing"
100% of the norm of the original `f`.

In that case, it suggests that an iterative process of considering
greater and greater `T_i` (selected as the gcd of the first `i` periods)
will give a better and better approximation of `f`. And, moreover, the
calculation of the weight for period `T_i` will be exact from the
beginning. It's harder to see how we should know which `T_i` to use, if
we don't know that by definition of `f`... But it suggests that, in
principle, we don't need the Fourier transform for mass decomposition.

**If We Don't Know Periods, When Is Fourier Mass Decomposition Safe Over
The Whole Real Line**

If one does not know the periods already, it would be nice to know when
the Fourier mass decomposition over the entire real line will work. In
the case of finite or countable linear combinations of sinusoidals, (a)
the Fourier transform at `\omega` should converge, and (b) when
reconstructing, we should converge in terms of "percentage" of L2 norm.

But can we detect when a function isn't such a sum?

Certainly if $\int_{-t}^{t} f(x) \conj{f(x)} dx$ grows too quickly in
`t`. The length traveled grows only linearly in `t` for any sinusoidal.
Even if the Fourier transform *did* converge for all `\omega`, certainly
the reconstruction could not approach the original in L2 norm.

I kind of suspect that it should work for any `f` with an L2 norm that
grows slowly enough. I think the fundamental question would be: if its
L2 norm grows slowly enough (and is non-zero), is there any `f` that is
orthogonal to all sinusoidals? That's because any "non-representable"
function consists of (1) a part that can be 100% represented and (2) a
part that is 100% *not* representable.

**Conclusion**

We need to move on to the true Fourier Transform, which is used for
decomposition of functions with finite L2 norm.

This discussion of "normalized inner product" here is very
idiosyncratic.
