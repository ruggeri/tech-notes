Let's finally begin considering the topological closure of our basis of
sinusoidals.

Any finite sum of sinusoidals with period `T/k_i` will have period
dividing `T`. It will be continuous and everywhere differentiable.

What about "infinite sums" of the basis vectors? Do these make any
sense? Maybe, if they converge, for some meaningful definition of
"converge."

**The L2 Norm**

The most natural topology to use is the L2 norm, which is the square
root of the inner product, which we have already defined. We could use
other norms or even more general topological definitions of convergence,
but this is not a topology course.

Let me give one justification for the L2 norm. The L2 norm basically
asks: how much of yourself would you want to use to reconstruct
yourself, according to this inner product? Say you have a sequence of
functions `f_k`, and these supposedly "converge" to a function `f`. What
would it mean if the L2 norm `||f - f_k||` didn't converge to zero? Then
that basically says: the amplitude of the residual is not collapsing to
zero. In that sense, the "reconstruction error" is not approaching zero.

(One can also note: the inner product multiplies amplitudes. The L2 norm
is exactly the norm that undoes that.)

**The Algebraic Closure Is Incomplete**

Our space of sinusoidals is not complete with respect to the sinusoidal
basis `{ \sqrt{2/T} sin(kt), \sqrt{2/T} cos(kt) }`. That is, there are
Cauchy sequences that don't converge to any algebraically spanned
function. I will later show that a sequence of sums converges to an
indicator function of `(a, b)`.

Note: even though an indicator function on `(a, b)` is spanned
topologically, that does not mean that there is a sequence that
pointwise converges to the indicator function. Consider the endpoints
`a, b`. The sinusoidal sequence will "jump" past those. This is called
the "Gibbs phenomenon." There is a finite jump of about 9%. But since
the jump is only at a couple points, it doesn't affect the L2 norm, and
the series still converges in norm to the indicator function.

What is the completion of the space? It is all of the L2 integrable
functions that have period `T`. This should be provable if I can show
(1) the basis spans (topologically) any indicator `(a, b)`, and (2) any
function topologically spanned by the sinusoidal basis must be square
integrable.

So the completion is `L2(0, T)`.
