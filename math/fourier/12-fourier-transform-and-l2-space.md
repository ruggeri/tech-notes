Let's remember that there *are* functions we want to do a Fourier
transform on, for which no Fourier series will give that function.
Consider a square integrable function on `[-\pi, \pi]` (with square
integral greater than zero).

Projecting that onto any `exp(i*\omega*t)` using our "fake" inner
product is going to give you a mass of zero. So for any *pulse*, the
Fourier series not only doesn't converge to the original function, it
actually tells you literally zero.

Hey Jonathan,

I've been reviewing my notes on the Fourier transform recently, and I
found myself interested in something.

When considering the Fourier *series*, you can consider the space of
square integrable functions on $[-\pi, \pi]$ to be a Hilbert space, with
your typical inner product and typical norm. The functions $exp(i*n*t)$
are a countable orthonormal basis. You can view finding the series as
just decomposing a vector in $L2[-\pi, \pi]$ into coordinates.

(Ah. I think I see where I've already gone a little wrong. I was
treating the inner product as $1/2\pi \int_{-\pi}^{\pi} f(t) \conj{g(t)}
dt$, to make sure that the $exp(i*n*t)$ were unit vectors. I probably
should have been normalizing the "unit" vectors, instead of changing the
inner product...)

Anyway, I went to consider the vector space spanned by the uncountable
basis of $exp(i*\omega*t)$. Note, this is *not* the space of square
integrable functions.

I knew that I'd eventually want to talk about integration. Otherwise,
what does it mean to take an "uncountable linear combination" of basis
vectors? But, to make things simpler, I started by considering
decomposing those vectors that were either a finite or countable sum of
the basis vectors.

To do this, I took what I thought was the natural extension of the inner
product I had been using before:

    \lim_{T -> \infty}
      1/T
      \int_{-T/2}^{T/2} f(t) \conj{g(t)} dt

This indeed does what I want for the basis vectors: they are all
orthonormal. Of course, for any function with compact support, I'm going
to get a "mass" of zero assigned to every $exp(i*\omega*t)$. But that
doesn't exactly bother me. I knew I would need integration.

I guess the thing I then found weird was that, using the "fake L2" norm
that corresponds to my "fake inner product," the basis vectors are
*discrete*. They are all at equal distance from each other: $\sqrt{2}$.
That makes total sense in retrospect; it should follow by orthonormality
of the basis.

But things feel really weird now. I need a continuous space of basis
vectors to be able to get anything out of using integration.

If I consider the true L2 norm, then the $exp(i*\omega*t)$ aren't even
part of the vector space, so they aren't a *basis* of the space. Things
are not neatly fitting into my plan of finding an orthonormal basis for
the L2 space.

I guess a question I have is: is there any extension of what we are
doing here that will handle both functions with finite L2 norm, as well
as the functions $exp(i*\omega*t)$ that do not?
