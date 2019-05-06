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

The real question is: what about `sin(k1t)` onto `sin(k2t)`? Is the
projection zero?

Here's how I see it. Say the first of these waves is traveling faster
than the second (one has to be if `k1 != k2`). Then for every rotation
that the first makes, the second falls `theta` radians behind (for
whatever theta).

So consider each time when the first wave is at phase `phi`. The
question is: what is the sum of the second wave's values at these times?

We know that these angles are all `theta` radians apart, and that they
create an entire circuit. Let `theta` correspond to some complex `c`.
We're basically asking: what is `1 + c + c^2 .. + c^{k - 1}`, where
`c^k` would equal one again.

(Note: you can jump ahead to the next section to see that repeated
rotations by `\theta` radians is equal to `cos(\theta) + i sin(\theta)`
exponentiated.)

The answer is zero. Why? Well, you can see by multiplying everything by
`c`. The 1 becomes `c`, but note that `c^{k-1}` becomes 1. So you get
the same thing. What does that mean? It means, whatever the sum is, when
rotated, it stays the same. And unless `c=1`, that means that the sum
must be zero.

If the sum is zero, both the real and the imaginary parts are zero. Thus
`sin(k1t)` is orthonormal to `sin(k2t)` and also the same for `cos`.

In conclusion, the basis is orthonormal. The projection operation
continues to be linear. Therefore, we have that the basis is
algebraically independent. (Even better, if we consider the topology
induced by the inner product, the basis is topologically independent,
too).

Inspiration: https://math.stackexchange.com/questions/891875/
