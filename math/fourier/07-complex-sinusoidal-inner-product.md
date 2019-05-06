Now that we have defined our one-dimensional vector space (with complex
scalars), let's see how to do an inner product here.

Given `c exp(it)`, we want to recover the `c` value. This is like
asking: how much does `c` rotate `exp(it)` extra? How would we calculate
this?

The answer is to rotate *back* by multiplying by `exp(-it)`. this will
leave you with `c`. Of course, this works out algebraically, too,
because the exponents sum to zero, which means `exp(0) = 1`.

In our simple case we are considering here, there is no need to
integrate. If we did integrate on `[0, 2pi)`, we'd just get `2pi c`. You
can see the correction factor we ought to have applied to our basis
vector is `\sqrt{1/T}`.

Why the difference from `\sqrt{2/T}` that we saw before? Maybe because
the real valued approach of projecting onto just `cos(theta)` only gives
you half the story, while now we get both the real and imaginary parts
of the story. (Actually, that's *exactly* the reason.)

Note that we multiplied by the *complex conjugate* of `exp(it)`. The
inner product of `f` and `g` is:

    \int f(t) \conj{g(t)} dt

Note that the conjugate is just the "unnormalized" inverse. Of course it
is: we just want to "rotate back."
