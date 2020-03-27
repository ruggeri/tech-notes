We'll start talking about *projection matrices*. This projects a point
into a subspace spanned by some vectors `a_i`.

The simplest projection matrices project onto a single vector `a`. To
do this, we know that `a\trans x` just takes the inner product of `a`
and `x`, which is how much we want to scale `a` to get the "closest"
point to `x` along `a`.

Let's take a quick note. We know that `<(x-<x,a>), a>=0`. That is, the
error is perpindicular to `a`. Note that this will *always* be part of
the error of `x - ra`, for any scalar `r`. So all we can do is
eliminate the component of the error along `a`, which minimizes the
distance to `x`.

Okay. So there needs to be a matrix for this projection operation,
because `Proj_a(u+v)=Proj_a(u)+Proj_a(v)`. I don't prove
this. Instead, I will just show you the matrix: it is `a a\trans`. That
describes exactly the operations I've described above.
