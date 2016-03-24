## Sinusoidal Basis

From linear algebra, I know that given an inner product and an
orthonormal basis, you can decompose a vector into components by
projecting onto each of the basis vectors. You can see the inner
product notes for that.

Let's take the sin/cosine functions with period dividing `2\pi` as a
basis. Is there a notion of an "infinite sum" of this basis? Yes, you
integrate over the basis, weighting by amplitude. Are these linearly
independent? I guess the answer is yes! Given those answers, we have
an infinite dimensional vector space.

The second question is how to do an inner product in this space. We
propose `\int_{-\pi}^{\pi} f(x)g(x)`. Note that this does the right
thing for projection onto the basis: the basis vectors are
orthonormal. One hint to prove this: the product of two periodic
functions is itself periodic, of course.

So the basis is orthonormal. And, also, the proposed product respects
scaling. In this way, I claim that this product "plucks out the
coordinates", where the coordinates are the sinusoidal basis.

Just like the dot product, this inner product is the only way to
extend the concept of "plucking out the coordinates" to an inner
product.

I think what we need is a better explanation for why this operation is
like "plucking out the coordinates" for a finite dimensional vector
space. Then everything should make more sense!

## Complex Numbers

**TODO2**: Why did I think this belonged here?

What is `i`? Let's talk about linear transformations on `R^2`. Then
these are representable in a natural way with 2x2 matrices.

Let's consider a subspace of these transformations: those which do
only scaling and rotation. Then I say these are of the form:

    scale * (vector that the first basis vector maps to)

For convenience's sake, we introduce `i`, which is the second basis
vector. Note that performing the linear transformation `i` twice is
the same as `-1`. In fact, we could show that we can compose linear
transformations through multiplication of these forms.

In particular, what is the square root of `i`? We could find it like
so:

    (a + bi)(a + bi) = i
    a**2 - b**2 + 2abi = i
    2abi = i (since a**2 - b**2 = 0 => a = b)
    ab = 1/2
    a**2 = 1/2
    => a = b = sqrt(2)/2

Another approach is to note that the square root of `i` is the linear
transformation that, performed twice, is equivalent to the
transformation `i` which is the ninety degree rotation. But that is
cleary a 45 deg rotation, which is what `sqrt(2) + sqrt(2)i` clearly
is.

## Euler's Formula

I don't know this yet:

    e^ix = cos(x) + sin(x)i

