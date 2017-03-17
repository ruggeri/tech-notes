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

If a matrix has `n` independent eigenvectors, you can perform an
*eigenvector decomposition*:

    A = S \Lambda S\inv

Where `S\inv` maps `e_1` to the first eigenvector, and `\Lambda` is a
diagonal matrix of eigenvalues. Basically, this says: (1) decompose a
vector into eigenvectors, (2) do the stretching, (3) reassemble.

If `A` has `n` eigenvectors with different eigenvalues, then they must
be linearly independent, which guarantees we can break `A` down like
this.

If we can break `A` down like this, it is easy to calculate powers of
`A`. This just involves exponentiating `\Lambda`, which is simple
because this is a diagonal matrix.

Likewise, it is trivial to invert! We should really have written `Q`
for `S`, since you should choose orthonormal eigenvectors. In that
case, it is trivial to invert `A`:

    A\inv = Q \Lambda\inv Q\trans

If `A` can be written like this, we call it *diagonalizable.* Not all
matrices are diagonalizable, however.

## Identifying Eigenvalues and Eigenvectors

Scaling a matrix won't change the eigenvectors, but will scale the
corresponding eigenvalues. Note that subtracting the matrix by `aI`
will reduce each eigenvector by `a`.

Note that any non-zero member of the null space is an eigenvector; an
eigenvalue can be zero. In fact, if `A - \lambdaI` has non-zero null
space, this says that lambda is an eigenvalue and there exists a
non-zero eigenvector. The nullspcae is exactly the eigenspace.

A way of identifying eigenvalues is to compute:

    det(A - \lambdaI)

When this is zero, lambda is an eigenvalue. Now, the above determinant
can be expressed as a polynomial equation in `\lambda` which is called
the *characteristic polynomial*. This zeros of this polynomial are the
eigenvalues.

The characteristic polynomial comes from the big formula; the term
which is `(a_11-\lambdaI)(a_22-\lambdaI)...(a_nn-\lambdaI)` is the
high order term.

The degree of the polynomial is `n`, so there are up to `n` distinct
roots of the polynomial. However, some roots may have multiplicity
greater than one. Also: some roots may be imaginary! Some polynomials
have *no real eigenvalues*!

A good example is a two-dimensional rotation by 90deg. This clearly
has no eigenvectors.

A matrix that can be eigendecomposed may have no inverse, because an
eigenvalue can be zero. Likewise, a matrix with an inverse may have no
eigendecomposition (see the shear mapping above) because an eigenvalue
is defective.

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

## Repeated Powers to find Eigenvector

You can find the eigenvector with greatest eigenvalue by choosing a
random vector and repeatedly applying the transformation. So long as
the random vector wasn't orthogonal to the greatest-eigenvalue
eigenvector, it must have some component in this direction. That part
will be expanded faster than any other part, so it converges to this.

I believe that after you find this, you can find the next largest
eigenvector by just projecting onto this eigenvector in the beginning
and subtracting the projection out. Now you repeat.

As mentioned, factoring the characteristic polynomial is not typical
because it is a "poorly conditioned" problem; it is very sensitive to
errors.

## Eigenspaces and Eigenbasis

There are up to `n` distinct eigenvalues, though there can be fewer if
an eigenvalue has multiplicity greater than one. The eigenvalues can
be complex, though.

Every eigenvalue has an eigenspace with dimension equal to the
multiplicity of the eigenvalue. The *geometric multiplicity* is the
dimension of the subspace of the eigenspace. The *algebraic
multiplicity* is a more fancy name for the typical multiplicity.

The geometric multiplicity can be less than the algebraic multiplicity
(though it must be at least one)! This is because an eigenvector can
be "repeated".

This is actually simple to happen. Consider a shear mapping, which
doesn't modify the first coordinate but maps the second coordinate to
`(k, 1)`. This has characteristic polynomial `(\lambda-1)**2`, so the
single eigenvalue is 1 (duh) and has algebraic multiplicity 2, but
geometric multiplicity 1. Weird!

An eigenvalue which has algebraic multiplicity greater than geometric
multiplicity is called a *defective* eigenvalue. A matrix with at
least one defective eigenvalue is a *defective matrix*. Defective
matrices are those which cannot be diagonalized, and thus have no
eigenbasis.

Maybe it goes without saying: an eigenbasis exists iff the matrix is
diagonalizable.

## Random thoughts

* Calculation: you solve the characteristic polynomial, then identify
  the nullspaces.
    * Wikipedia warns that proper calculation of eigenvalues through
      factoring the polynomial can be hard because small round-off
      errors on the coefficients of characteristic polynomial can have
      big errors on the roots.
* Product of the eigenvalues is naturally the determinant of the
  matrix. Because a box along the dimensions of the eigenvectors would
  be scaled appropriately.
* Imaginary eigenvalues come in pairs: they are complex
  conjugates. This is true only if the characteristic polynomial has
  only real coefficients, which is true if the matrix has only real
  entries.
    * This is why a square matrix with odd dimension must have at
      least one real eigenvalue.
* Eigenvalues of a digonal matrix are trivial (the diagonal elements),
  as are corresponding eigenvectors (the corresponding basis vector).
* Eigenvalues of a triangular matrix are also those along the
  diagonal.

**TODO**: Investigate characteristic polynomial.
