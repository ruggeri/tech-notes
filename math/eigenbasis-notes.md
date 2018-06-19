**First**: The definition of a *Hermitian* matrix is that `A` is equal
to its conjugate transpose. We don't know what that "means" though, yet.

**Second**: A Hermitian matrix operation is *self-adjoint*:

    <Au, v> = <v, Au>

for the reason:

    <Au, v> = vT*A*u = uT*AT*v (transpose of scalar = scalar)
    = uT*A*v = <u, Av>

At present this is just wizardy with symbols.

**Third**: A self-adjoint operation has an *orthogonal diagonalization*.

The characteristic polynomial for A has at least one solution, by
the Fundamental Theorem of Algebra. Call this `lambda1`.

Therefore, there exists `e1` such that `Ae1 = lambda1 e1`. So:

I claim that any vector `v` orthogonal to `e1` must imply `Av` is
orthogonal to `e1` too. Because:

    <Av, e1> = <v, Ae1> (A is self-adjoint)
    = <v, lambda1 e1> = conj(lambda1) <v, e1> (by e1 an eigenvec)
    = conj(lambda1) * 0 (by presumed orthogonality of v and e1)

Therefore, we see that the matrix `A` is a self-adjoint operation on the
subspace perpindicular to `e1`. Therefore we may repeat the process.

This gives us `n` orthogonal eigenvectors for an `n`-dimensional space,
as desired.

Additionally, every eigenvalue is real. This is because:

    <Ae1, e1> = <lambda1 e1, e1> = lambda1 <e1, e1>
    OR
    <Ae1, e1> = <e1, Ae1> (self-adjoint)
    = <e1, lambda1 e1> = conj(lambda1) <e1, e1> (by inner product conugate symmetric)

So any self-adjoint operation, even a complex one, is "stretching" an
orthogonal basis for the space.

Thus we may say:

    A = Q D Q^H (where Q^H is the conjugate transpose)

This is because `Q^H` projects vectors onto the basis, action by `D` is
to stretch, and `Q` returns this to the original coordinate system.

**Fourth**: Any self-adjoint operator must be Hermitian. That is, if `A`
is self-adjont, then `A = A^H`.

We already know that `A` must have an orthogonal diagonalization (aka
eigenbasis decomposition). Further, the algebra of the eigenbasis
decomposition shows that `A` must be `(QDQ^H)^H = QDQ^H`. This relies on
`D` being real. I thought that real eigenvalues was irrelevant, but
curious...

Alternatively, we can see that:

    A = sum_i lambda_i e_i e_i^H

Notice that this is a sum of projection matrices for orthogonal eigen
vectors `e_i`. I need `e_i^H` to do the projection; if `e_i` has complex
values, then the complex conjugate is necessary because that's how
projection works with complex numbers.

**Fifth**: We are now full-circle:

    Hermitian matrix => Self-adjoint operator => Orthog Diagonalization => Hermitian matrix

**Questions**

The first mystery is why `A=A^H` implies self-adjoint. This is clear
from the algebra, but not intuitively clear to me in terms of a story.

To discover why, I think it is worth exploring which matrices are
self-adjoint and then figure out why they are Hermitian. For instance,
we may consider the matrix which rotates `e_1` toward `e_2` by `theta`
radians, and `e_2` toward `e_1` by `theta` radians.

It seems that self-adjacency is the key to orthogonal eigenvector
decomposition. I would like to consider some matrices which don't have
orthogonal eigenvectors (or maybe don't even have all of them). I would
like to think about this idea that vectors orthogonal to an eigenvector
stay perpindicular to the eigenvector upon transformation.
