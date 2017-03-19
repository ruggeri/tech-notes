## Top Level

tl;dr. Real symmetric matrices have all real eigenvalues. Real
symmetric matrices can always be diagonalized by an orthogonal
matrix. That is: a symmetric matrix is the result of pulling on a set
of orthogonal basis vectors. (This is the spectral theorem).

Why?!

(Boring note, when we have geometric multiplicity >1, we *could*
choose non-orthogonal vectors, but the spectral theorem says we can
always choose orthogonal eigenvectors).

## Ortho Eigenbasis => Symmetric

A transformation that can be diagonalized by an orthonormal eigenbasis
must be symmetric. The algebra is simple:

    A = S \Lambda S\inv = Q \Lambda Q\trans
    A\trans = Q\trans\trans \Lambda\trans Q\trans
    = Q \Lambda Q\trans

That was easy!

**TODO**: What's the intuition?

## Symmetric => Eigenvalues are Real

Here's the deal. Consider an `x` such that:

    Ax = \lambda x

Because `A` is real, then even if `x` is imaginary, then `x\conj` is
also an eigenvector for `\lambda\conj`, also an eigenvalue:

    Ax\conj = \lambda\conj x\conj

So, let's multiply the first by `x\conj\trans` on the left, and the
second by `x\trans` on the left. Note that I'm just going to use the
inner product on the right side because this is symmetric anyway.

    x\conj\trans A x = \lambda (x \cot x\conj)
    x\trans A x\conj = \lambda\conj (x \cdot x\conj)

Now, we use symmetry on the second by taking its transpose (doesn't
change the right, just a number):

    x\conj\trans A\trans x = \lambda\conj (x \cdot x\conj)
    x\conj\trans A x = \lambda\conj (x \cdot x\conj)

Notice how I've dropped the transpose on the `A`?

We've made it! We now have two different right hand sides for the lhs
`x\conj\trans A x`. This is:

    \lambda (x \cdot x\conj) = \lambda\conj (x \cdot x\conj)

This shows that `\lambda` must be real!

**TODO**: The intuition must have to do with the fact that a symmetric
matrix is related to some kind of inner product.

**TODO**: I also think that having more intuition about vector spaces
over a complex field would be helpful.

## Hints

Self-adjoint. I think that means that you put me back together the
same way you decompose me.

Inner product `<Ax, y> = <x, Ay>`. Note. This is a property that
depends only on the innerproduct and the transformation defined by
A. Interestingly, A will be symmetric in any basis.x

Could look up this "polar decomposition." This says any non-singular
matrix can be decomposed to a product of an orthogonal matrix and a
symmetric matrix.

## Resources

I've read Strang's treatment in the book but it wasn't super helpful.

* Axler book. Lang book.
* Ask people on FB.
* Study back on vector spaces over complex fields.
* Pursue this notion of the inner product imbued by A.
* Continue toward SVD, and hope this helps my intuition?
* Strang videos; watched #25 (well, up to when he finishes with
  spectral theorem and starts positive definite; he doesn't prove
  spectral theorem). #26 might help (it's on complex matrices).
