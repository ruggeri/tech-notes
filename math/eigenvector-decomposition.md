Hey Jonathan!

I think I finally understand why a real symmetric matrix must be
diagonalizable. (I will only talk about real matrices here).

Unrelatedly: a diagonalizable matrix must be symmetric, since it is
the sum of $lambda_i u_i u_i^t$.

**2x2 Real Symmetric Matrices Have At Least One Eigenvector**

I first proved to myself that every 2x2 real symmetric matrix must be
diagonalizable. I used some continuity arguments (and a bunch of
cases). For instance, if $Ae_1, Ae_2$ both lie in the first quadrant,
then consider $v_\theta = cos \theta e_1 + sin \theta e_2$. Then as
$\theta$ sweeps through $(0, pi/2)$, it by necessity must eventually
pass through $Av_\theta$, and thus there must be an eigenvector.

Of course, there are a bunch of other cases. To give a taste of
another: if $Ae_1$ is in the first quadrant but $Ae_2$ is in the
fourth, either (1) $A v_\theta$ sweeps clockwise through the first
quadrant as $v_\theta$ sweeps counter-clockwise, or (2) $A v_\theta$
sweeps CCW through the entirety of Q3, but that implies that
$-A_v\theta$ sweeps CCW through all of Q1, eventually "overtaking"
$v_\theta$.

Anyway, by enumerating such cases, I've shown that a single
eigenvector exists.

**2x2 Full-Rank, Real Symmetric Matrices Have Two Orthogonal
Eigenvectors**

After finding $\lambda_1, u_1$, we may look at $A - (\lambda_1 u_1
u_1^T)$. The difference of two symmetric matrices is symmetric. This
"reduced" matrix has rank one, because ${\alpha u_1 | \alpha in R}$ is
in the null space. Therefore, the reduced $A$ is a projection matrix,
and we have identified the 2nd, orthogonal eigenvector.

**Change of Basis Maintains Symmetry of Matrix**

Consider $B = Q^T A Q$, where $A$ is a symmetric matrix, and $Q$ is a
rotation matrix. Then of course $B^T = Q^T A^T (Q^T)^T = Q^T A Q = B$.

Then the preceeding statement about 2x2 matrices is a special
case. For if we have identified one eigenvector, write it as the first
row of $Q$, and chose any second row that is orthogonal. Then clearly
the first column of the rotated matrix $A'$ is $(\lambda_1, 0)$. But
since $A'$ must still be symmetric, it follows that it is
diagonal. And thus we have that the second column is $(0, \lambda_2)$
and we have our second eigenvector in the second row of $Q$.

**Inductive Step**

Let us assume that every $n$ dimensional symmetric matrix is
orthogonally diagonalizable. (Our inductive hypothesis.)

Let us claim (**to be proved later**) that every $n+1$ dimensional
symmetric matrix possesses **at least one** eigenvector. We want to
show that the $n+1$ dimensional symmetric matrix has all $n+1$
orthogonal eigenvectors.

Then write that eigenvector into the first row of $Q$, and choose $n$
additional orthogonal eigenvectors however you like.

Applying the change of basis to the original $A$, we zero out the
first row and column of the new $A$, except for a one in the top
left. The rest of the matrix is whatever, **but it is of course still
symmetric**.

We may now entirely disregard the first row/col of $A$, and work in
the reduced version, $A'$. By the inductive hypothesis, $A'$ is
diagonalizable by a rotation matrix $Q'$. But then we can multiply $Q'
Q$ (where $Q'$ is extended back again to $n+1$ dimensions by adding a
top-left one), which gives us our diagonalization of the original
matrix.

Of course, this all relied on us proving that there exists **at least
one** eigenvector for the original symmetric matrix $A$.

**Producing At Least One Eigenvector for A Real Symmetric Matrix $A$**

I will construct a series of rotations $Q_i$ such that:

    A = Q_1^T ... Q_{n-1}^T A' Q_{n-1} ... Q_1

and $A'$ has empty first row and first column, except for a top-left
$\lambda_1$. If I do that, then note that I have acheived an
eigenvector which is the first row of $Q_{n-1} ... Q_1$.

Here is what I do. I start with the reduced 2x2 matrix:

    [ a_{1, 1}, a_{1, 2} ]
    [ a_{2, 1}, a_{2, 2} ]

Because of symmetry of $A$, we know that $a_{2, 1} = a_{1, 2}$. Which
means that because 2x2 real symmetric matrices are always
diagonalizable, we know that there exists $Q_1$ which orthogonalizes
this reduced matrix.

(BTW: I did come up with the equation for the required rotation. I
believe that is $tan 2\theta = \frac{2 a_{1, 2}}{a_{2, 2} - a_{1,
1}}$. I was very proud to prove this on the plane home to New York!)

Well, we're in good shape. Simply extend $Q_1$ to be an $n$-by-$n$
dimensional matrix by writing in 1s on the extended diagonal. This
clears out both $a_{1, 2}, a_{2, 1}$ as desired. Note that only the
first two rows of $A$ are affected.

This specifically chosen *Givens rotation* (a name for any rotation in
a plane subspace) is sometimes called a *Jacobi rotation* (the one
which eliminates $a_{1, 2}, a_{2, 1}$.

We may iterate this process, applying it to rows one and two, then one
and three, then one and four... Each rotation clears out (1) another
entry in the first column, (2) the corresponding entry in the first
row (because symmetry is maintained throughout), (3) without changing
any other entry (excepting $a_{1, 1}$).

After doing $n-1$ such rotations, we have eliminated the first column
excepting $a_{1, 1} = \lambda_1$. And this shows that the first row of
$Q_1 ... Q_{n-1}$ is an eigenvector.

We are thus done. We may activate the inductive hypothesis to know
that the rest of the eigenvectors exist.

**Continuing the Schur Decomposition Of A Symmetric Matrix**

Note that there is really no need for (abstract) induction. We may
just continue the process above with pairs of the second and third
rows, second and fourth tows..., third and fourth rows, third and
fifth rows...

If we continue to do this, we will fully decompose the matrix $A$ into
$Q^T D Q$, just as we wanted.

(Note: even though this exactly works, proceeding in this order may
have very poor numerical stability. But I am not concerned with that
practical consideration at present. Indeed, my understanding is that
you don't want to use Schur decomposition anyway for finding
eigenvectors of a matrix...)

**Triangularization Of A Matrix**

We were able to choose Jacobi rotations that eliminated both $a_{1,
2}$ and $a_{2, 1}$ in the **symmetric** matrix

    [ a_{1, 1}, a_{1, 2} ]
    [ a_{2, 1}, a_{2, 2} ]

What if the matrix is *not* symmetric? Then there is still the ability
to eliminate the $a_{2, 1}$ entry via a rotation. We could
alternatively eliminate $a_{1, 2}$, so eliminating $a_{2, 1}$ is a
convention. But if this 2x2 matrix is not symmetric, we aren't
guaranteed to be able to eliminate *both* $a_{1, 2}$ and $a_{2, 1}$.

An example:

    [ 1 1 ]
    [ 0 1 ]

This is a shearing transformation. It clearly has an eigenvector of
$e_1$, but no second eigenvector (since $e_2$ is getting smashed into
$e_1$).

In the case where $A$ is not necessarily symmetric, we can still
decompose $A$ into

    A = Q^T U Q

where $U$ is an **upper-triangular** matrix. Of course, if $A$ is
symmetric, then so is $U$, and thus $U$ must be diagonal.

Since this process works for *any* (real) matrix, every matrix is
*triangularizable*. (Probably every complex matrix too...).

**TODO**: Am I sure of this? Can rotation matrices be written as a
Schur decomposition?

**Improving Numerical Stability**

We can improve the stability of the Jacobi eigenvalue algorithm if we
eliminate entries in order of descending magnitude. But in that case,
we will not proceed in the proper order that is guaranteed to
eliminate all non-diagonal entries.

However, as we continue to apply the rotations, the magnitude of the
off-diagonal rotations will drop, even as the eigenvalues on the
diagonal approach their correct values. Thus, the approximation which
throws away the remaining (small) off-diagonal entries should approach
the correct diagonalization we want.

Again, I believe that the other eigenvector finding algorithms (such
as the "QR" algorithm) are faster. But I believe an advantage of this
Jacobi approach is that it is parallelizable.

Note: you probably want to do something a lot smarter if you have
*sparse* matrices. And working with dense matrices, I assume you can't
handle very many dimensions, since the number of entries grows
quadratically...
