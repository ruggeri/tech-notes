## The Linear Algebra Picture

From linear algebra, I know that given an inner product and an
orthonormal basis, you can decompose a vector into components by
projecting onto each of the basis vectors. This is proven like so:

**Projection on Orthonormal Basis**

Given a basis for the vector space `{e_i}`, we want to write a vector
`v` in terms of the `{e_i}`: `v = sum c_i e_i`, for `c_i \in \R`.

To find the `c_i`, we need an **inner product**. Assume the standard
inner product. We can further assume that that the `e_i` are
orthonormal, since we can always orthonormalize (Graham-Schmidt).

Then `c_i` must equal `v \cdot e_i`, since:

    v \cdot e_i = (sum (c_i e_j)) \cdot e_i = sum (c_i (e_j \cdot e_i)) = c_i

QED!

As before (and in my linear algebra notes), I can explain why the
standard inner product makes sense. Basically, the standard inner
product is exactly what you get if you want `<te1+(1-t)e_2, e_2>=t`
and `<te1+(1-t)e_2, e_2>=1-t`. Any choice of independent basis implies
a topology/inner product in the natural way by writing everything as
coordinates in this basis and use the standard inner product.

## The Sinusoidal Basis and Inner Product

Let's take the sinusoidal functions as a basis. The first question is
whether these are independent. Let's assume so.

The second question is how to do an inner product in this space. We
propose `\int_{-\pi}^{\pi} f(x)g(x)`. How do we verify this is an
inner product?

Let's think first of projecting `sin(x)` onto itself. This is
`\pi`. This is likewise true for `cos(x)` projected onto itself. This
is true for any period sinusoidal. So I should scale by `1/pi`.

So the basis is given unit length. Next, we consider sums of basis
vectors. The next critical point we need is that:

    \int sin(mx)cos(nx) = 0 = \int sin(mx)sin(n) = \int cos(mx)cos(nx)

When `m!=n`. This proves that basis vectors are mapped to be
orthogonal.

Now, by linearity of integration, we are done. This holds because any
the inner product of a linear combination of the sinuosidal basis can
be projected onto a basis vector to recover the amplitude of that
basis vector.

This is effectively what the Fourier transform is going to do.

TODO! Details I need to fill out:

* Explain why sinusoidal basis is independent.
* Explain why it makes sense that this inner product should respect
  the basis and have the correct products of zero.

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

