The purpose of the Fourier transform is clear to me. Give a function
defined in the time domain, we want to rewrite it as a weighted
infinite sum of sinusoidal functions.

Say the period of the function is 2pi seconds. Then:

    f(t) = a_0 \int a_i sin(nx) + \int b_i cos(nx)

The trick is finding the appropriate `a_i` and `b_i`.

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

## The Big Picture

From linear algebra, I know that given an inner product and an
orthonormal basis, you can decompose a vector into components by
projecting onto each of the basis vectors. This is proven like so:

**Projection on Orthonormal Basis**

Given a basis for the vector space `{e_i}`, we want to write a vector
`v` in terms of the `{e_i}`: `v = sum c_i e_i`, for `c_i \in \R`.

To find the `c_i`, we need an **inner product**. Assume one. We can
further assume that that the `e_i` are orthonormal, since we can always
orthonormalize (Graham-Schmidt).

Then `c_i` must equal `v \cdot e_i`, since:

    v \cdot e_i = (sum (c_i e_j)) \cdot e_i = sum (c_i (e_j \cdot e_i)) = c_i

QED

Okay. So the inner product we use in function space is:

    \int f(x)g(x) dx

This is a clear generalization of the `R^n` inner product. It is no
less intuitive than the standard inner product. **TODO2: Which is a
complete fucking mystery to me**.

This sinusoidal functions are an orthonormal basis. **TODO2: What is
the intuition behind that?**

## Insight

I think the standard inner product makes more sense now. Basically,
what is an orthonormal basis? Well, you pick a basis, and you declare
it orthonormal; your basis gives you the geometry of angles. Angles
come from saying that as you vary `t`, then the angle formed by
`te_i + (1-t)e_j` to `e_i` sweeps from zero to one.
