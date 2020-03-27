## Eigenvector and Eigenvalue

*Eigenvectors* obey this rule:

    Ax = \lambda x

The `\lambda` is an *eigenvalue*. Basically, the eigenvectors are
being stretched by a factor of `\lambda`.

Each eigenvector actually has a whole line of eigenvevectors
associated with it. We can just use a convention of choosing a single
norm 1 vector to represent this line.

In the case where two independent eigenvectors have the same
eigenvalue, there is a plane of eigenvectors. In fact, we can talk
about the *eigenspace* of an eigenvalue, which is the space of vectors
all scaled by this eigenvalue.

## Eigenbasis and Eigendecomposition and Diagonalizable

If a matrix has `n` linearly independent eigenvectors, this is called
an *eigenbasis* for the matrix. In that case, you can perform an
*eigendecomposition* of the matrix:

    A = S \Lambda S\inv

where `S\inv` maps `e_1` to the first eigenvector, and `\Lambda` is a
diagonal matrix of eigenvalues. Basically, this says: (1) decompose a
vector into eigenvectors, (2) do the stretching, (3) reassemble.

If `A` can be written like this, we call it *diagonalizable.* A matrix
is diagonalizable iff it has an eigenbasis. (If we mean that S has all
real entries, then we ought to say that a matrix is diagonalizable a
basis of real eigenvectors exists).

If we can break `A` down like this, it is easy to calculate powers of
`A`. This just involves exponentiating `\Lambda`, which is simple
because this is a diagonal matrix. Likewise, it is simple to invert
`A`.

A matrix that can be eigendecomposed may have no inverse, because it's
okay if an eigenvalue can be zero, as it still has a corresponding
eigenvector. Likewise, a matrix with an inverse may have no
eigendecomposition because it is defective (see below about the shear
mapping).

## Identifying Eigenvalues and Eigenvectors

Scaling a matrix won't change the eigenvectors, but will scale the
corresponding eigenvalues. Note that subtracting the matrix by `aI`
will reduce each eigenvector by `a`.

Note that any non-zero member of the null space is an eigenvector; an
eigenvalue can be zero! In fact, if `A - \lambdaI` has non-zero null
space, this says that lambda is an eigenvalue and there exists a
non-zero eigenvector. The nullspace is exactly the eigenspace for this
eigenvalue.

A way of identifying eigenvalues is to compute:

    det(A - \lambdaI)

When this is zero, lambda is an eigenvalue. Now, the above determinant
can be expressed as a polynomial equation in `\lambda` which is called
the *characteristic polynomial*. This zeros of this polynomial are the
eigenvalues.

The characteristic polynomial comes from the big formula; the term
which is `(a_11-\lambdaI)(a_22-\lambdaI)...(a_nn-\lambdaI)` is the
high order term. The degree of the polynomial is `n`, so there are up
to `n` distinct roots of the polynomial.

Once you have the lambda, it is simple to calculate the nullspace of
`A - \lambdaI` in the typical way.

## Problems

If the matrix has all real entries, real eigenvalues must have a
corresponding real eigenvector.

However, there are imaginary roots to the characteristic polynomial!
In the case that the matrix is real, I believe that this means the
eigenvectors must have imaginary component. This is one way to fail to
have an eigenbasis of real vectors.

A good example is a two-dimensional rotation by 90deg. This clearly
has no real eigenvectors.

Some roots of the characteristic polynomial may have multiplicity
greater than one! This is called the *algebraic multiplicity* of the
eigenvalue. However, the *geometric multiplicity*, which is the
dimension of the corresponding eigenspace, can be *less than* the
algebraic multiplicity! This is really weird, because we just plain
*lose* eigenvectors; they're not even imaginary eigenvectors.

Even in this case, every eigenvalue must correspond to at least one
eigenvector, per the definition of eigenvalue.

**TODO**: I want a proof. Is a complex root of the characteristic
polynomial necessarily an eigenvalue? I know what the determinant of a
real matrix is, which means I know that a real eigenvalue implies the
existence of a dimension which is lost. But I don't really know what
the determinant of a complex matrix would really mean.

If `AM > GM`, we call this a *defective eigenvalue*. In that case, the
entire matrix is called *defective*.

A defective matrix has no eigenbasis for the full space, so it has no
way to possibly be diagonalizable.

## Matrices with all distinct eigenvalues are always diagonalizable

On the other hand, if a matrix has all distinct eigenvalues, it must
have an eigenbasis, because each of `n` eigenvalues has geometric
multiplicity at least one (in fact, equal to one in this case) thus
each has an eigenvector.

The reason is that if you could break up an eigenvector into a linear
combination `u=av + bw` of two other eigenvectors. Since we can always
scale up or down eigenvectors, let's choose `v,w` specifically so `u =
v + w`. Then `Au = \lambda_v v + \lambda_w w`, which shows that `Au`
is no multiple of `v+w` like it should have been.

In the case where the matrix is real and the eigenvalues are real,
this means a real eigenbasis.

Note: you don't need distinct eigenvalues to be
diagonalizable. Consider the identity matrix.

## Defective Matrix Example

A rotation has imaginary eigenvalues and imaginary eigenvectors. I
think I can kinda see where this comes from. Vectors with imaginary
components have been partially rotated somehow so they can be
perpindicular to an axis of rotation.

I'm more worried defective matricese. Consider a shear mapping `A`:

    1, k
    0, 1

Then `A - \lambdaI` is:

    (1-\lambda) k
    0           (1-\lambda)

The determinant is then `(1-\lambda)**2 - k0`. This has a root of 1
with multiplicity two. However, the geometric multiplicity is only
one. We know any eigenvector must be real because this is a real
matrix with a real eigenvalue. However, it is clear by looking at the
transformation that the only such eigenvector is `(1, 0)`.

Very weird!

## Repeated Powers to find Eigenvector

You can find the eigenvector with greatest eigenvalue by choosing a
random vector and repeatedly applying the transformation. So long as
the random vector wasn't orthogonal to the greatest-eigenvalue
eigenvector, it must have some component in this direction. That part
will be expanded faster than any other part, so it converges to the
greatest stretched eigenvector.

I believe that after you find this, you can find the next largest
eigenvector by choosing a random vector and subtracting out the
component parallel to the first eigenvector. Now you repeat the above
algorithm to find the second greatest!

In practice, factoring the characteristic polynomial is not typical
because it is a "poorly conditioned" problem; the roots calculated
will be very sensitive to round-off errors in the terms of the
coefficients of the characteristic polynomial.

## Difference Equations

Eigenvectors are useful for any state which is updated by the same
matrix over a series of steps. This is called a *difference
equation*. Two examples:

* Markov matrix. This is a transition matrix. You start with a
  probability distribution, and then you repeatedly apply the
  transition matrix at each turn
* Fibonacci numbers. Each step takes a vector of the past two
  Fibonacci numbers. So you have a matrix that adds them together for
  the first coordinate, and swaps the first coordinate into the new
  second coordinate. The eigenvector of this matrix shows you the
  limiting ratio of the two numbers!

## Random thoughts

* Eigenvalues of a digonal matrix are trivial (the diagonal elements),
  as are corresponding eigenvectors (the corresponding basis vector).
* Eigenvalues of a triangular matrix are also those along the
  diagonal.
* Product of the eigenvalues is naturally the determinant of the
  matrix. That is clear if all eigenvectors exist and are real,
  because a box along the dimensions of the eigenvectors would be
  scaled appropriately.
* Assuming the matrix is real, then imaginary eigenvalues come in
  pairs: they are complex conjugates.
    * Proof: `Ax=\lambda x => A\conj x\conj = \lambda\conj x\conj`.
    * Then: `Ax\conj = \lambda\conj x\conj`. We drop the conjugate on
      `A` because A is real. This shows that an imaginary
      eigenvector's conjugate is also an eigenvector, and its
      eigenvalue is conjugate, too.
    * A matrix with odd dimension must have at least one real
      eigenvalue!

## TODO

**TODO**: I don't understand exactly when a real matrix would have
imaginary eigenvalues and eigenvectors. I assume it's when some part
of the matrix operates as a rotation of some subspace.

This doesn't worry my intuition terribly because the matrix is still
diagonalizable if we simply accept an eigenbasis with imaginary
eigenvectors.

**TODO**: I don't understand when a matrix is defective. That just
feels creepy.
