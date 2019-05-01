We would like to consider *uncountable sums* of our basis vectors. What
could that even mean?

When we're talking about uncountable sums, we're talking about
integration. What do we need for integration? First, we need a *measure*
on open sets in the space from which we are summing up. Then we need to
assign a *density* to each point in the space.

For the notion of convergence to make sense, we need a sequence of
finer-and-finer approximations. We achieve this by choosing
finer-and-finer partitioning of the space being integrated over.

Last, we need a norm, so that the idea that a sequence can converge
makes sense.

(I'm probably not speaking very accurately about Lebesgue integration,
but give me a break.)

Now, if we want to decompose functions into the Fourier basis, assigning
a "density" for each basis vector `exp(i*\omega*t)`, then we need a
notion of proximity in our basis. But the inner product/norm I used in
the prior section won't help.

The reason is that the Fourier basis is *discrete* with respect to the
norm I introduced. You might suspect that this is the case, because the
inner product of two basis vectors is always `0.0`. If you do the math
(and I have at the whiteboard), you'll see that my "fake" L2 norm from
the previous section evaluates to `\sqrt{2}` for any two sinusoidals
with different periods. That makes sense: `||e_2 - e_1||_2 = \sqrt{2}`
sounds like it should be correct.
