We've considered the space spanned by `sin, cos`. But what if we want to
extend this? We're only talking about functions with a period of exactly
`2\pi`. What about functions that are *periodic* on `2\pi`? That is, the
algebraic closure of:

  cos(kt), sin(kt) (k a non-zero positive integer)

Note that now we have the possibility of infinite sums. This is an
*infinite dimensional vector space*. This brings in a *topological*
question. As in: what does it mean for an infinite series to converge?
But let's just stick to finite linear combinations of the basis vectors
to start. We'll get back to topology later.

First question: are the basis vectors linearly independent? I should
probably show that the answer is yes. But I'll assume it, and we'll see
why this is so in a bit.

Let's jump to the inner product. Are the basis vectors orthogonal?

We know that `cos(k1 t)` projected onto `sin(k2 t)` continues to be
zero. The same argument about integrating an odd function applies.

What about `sin(kt)` projected onto `sin(kt)`? This is still `\pi`,
because you make `k` revolutions around, but you're making them `k`
times more rapidly. So the total remains the same and so does the
correction factor: `\sqrt{2/T}`. The same argument of course applies to
`cos(kt)`.

**Sinusoidals With Different Periods Are Orthogonal**
