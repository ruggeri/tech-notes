```
var v = document.querySelector('video'); v.playbackRate = 2.5
```

## Determinant

The determinant is the volume that the unit cube is mapped into. Note
that, by definition of a linear transformation, volume is homogenously
scaled regardless of the shape.

When we have `LUP` form, the determinant is simple to calculate. `P`
changes the sign possibly, but not the magnitude. Then `trace(U)`
scales the vector, followed by `trace(L)`. This is because the other
components are "skews" which don't change the volume. Another way:
consider `A_11` in a upper triangular matrix. This scales the first
basis vector by this much. Now consider the next column; we can ignore
the first component of the second column, since this pushes `e_2`
along `f(e_1)`, which is a skew that adds no volume. The magnitude *of
the orthogonal part* is `A_22`. Etc.

**TODO2**: The trace is actually the sum of the diagonal. I mean the
product.

## Invertibility

Note that if `L` has a zero on the diagonal, then it maps some basis
vector into a subspace of the prior columns. That is, this column has
no component orthogonal to the subspace defined by the previous
vectors. That's why the deteriminant is zero. For the same reason, the
matrix cannot be inverted invertable. We call matrices with zero
determinant *singular*.

It is worth noting that singular matrices are sparse in the space of
matrices with random coordinates.

## Preimage of singular matrices

Say the matrix doesn't have full column space. Then its nullspace has
non-zero dimension. To find the nullspace, invert the matrix with the
bad columns removed. Find the inverse under the transformation of the
opposite of each column. This preimage plus the column is a basis
vector in the nullspace.

Likewise, to find the pre-image of a vector, take the inverse of the
matrix with the dependent columns removed. Apply this inverse to the
vector. This, plus an element of the nullspace is in the preimage of
the vector.

## Orthogonality and inverses

What if `A` does not have full row rank? That is, what if the matrix
doesn't span the entire space, so a vector may not have an inverse?
Let us say we want `Ax=y`, but `y` doesn't lie in the columnspace of
`A` and thus no solution exists. In that case, let us project `y` into
the columnspace, resulting in `y\hat`. Note that `y-y\hat` is
orthogonal to the columnspace of `A`, so in some sense this is the
best we can do. We then solve for `y\hat`.

How do we get `y\hat`? If the columns of `A` are orthogonal, unit
vectors, we can project `y` onto each of the columns of `A` to get
`y\hat`: `y\hat := (A\trans)y`. But if the columns of `A` are not
orthonormal, we can still solve `(A\trans)A(x\hat)=(A\trans)y`.

Note a special case if `A` is orthogonal, but not of full column
rank. Then, `A\transA=I` (rectangular version), and we can write
`x\hat=(A\trans)y`.

This basically says that we project `y` into the columnspace, but we
realize this did some stretching of `y` (since `A` may not be
orthogonal), so compensate by doing similar stretching of `A(x\hat)`.

Equivalently, you can say: `x\hat=(((A\trans)A)\inv)(A\trans)y`. Note
that `(A\trans)A` is an `n`-by-`n` matrix; it has full rank and is
invertible if `A` has full column rank. Basically it is undoing the
stretching and then inverting back into the original space.

Note that to project into the columnspace of a matrix, we can multiply
`x\hat` as solved above by `A` again. Just to note: this is simple
when `A` has orthonormal columns (but maybe not full row rank), since
it is `Q(Q\trans)`, since the `(Q\trans)Q` cancels out!

This comes up when you have many data points and are trying to fit a
linear model. What we've done corresponds to OLS regression. Note that
OLS doesn't have to be linear; you can always add quadratic terms and
it's still a "linear" model.

## Graham-Schmidt

I already know Graham-Schmidt. You decompose `A=QR`, by iterating
through the columns, each time subtracting out the projection of the
previous columns. This gives you an orthogonal component `Q`, and a
matrix `R` which reflects the changes you made.

My understanding is that `LU` is preferred to `QR` for inversion,
since `QR` takes about twice as many operations. But I think `QR` is
found useful when you don't have a square matrix (which happens for
instance when you want to do an SVD), which `LU` decomposition
requires. I also read that QR is more numerically stable.

## Determinant formula

Because of linearity, you can break up determinant into a sum of
determinants, where each matrix has 1 entry from each column. The
number of such matrices is `n!`. The determinant of this is clearly
the product of the entries times the sign. This is the "big
formula". Of course, it is equivalent to do the QR decomposition.

Also note that we can work an item at a time: this is the cofactor
formula. When we choose an item from the first column, we still have
all possible permutations of the other items. This is the determinant
of the *minor*, the submatrix with the appropriate column and row
removed.

## Eigenvector

An eigenvector has `Ax=(\lambda)x`. Eigenvalue is a value `\lambda`
such that there exists `x` where `Ax=(\lambda)x`. E.g., a projection
matrix has as its eigenvectors the columnspace, where the eigenvalue
is 1. Those vectors normal to the columnspace are also eigenvectors
with value 0. (BTW: by "projection matrix" I guess we mean it has
orthonormal columns).

Note that eigenvector satisfies `(A-(\lambda)I)x=0`. That is, `x` lies
in the nullspace. That means that the determinant must be zero. Then
we can use the determinant formula to find `\lambda`. Plugging this
in, we can then find the nullspace.

Your eigenvalues can be *complex* even when the matrix is
real. Consider, of course, the rotation matrix.

**NOTE**: Eigenvalues of a triangular matrix are the diagonal
elements. Also weird: if you have duplicated eigenvalues, you may not
have an independent eigenvector for each.

Note that if we write the eigenvectors of `A` in the columns of a
matrix called `S`, then `AS=SL`, where `L` has the eigenvalues in the
diagonal. Provided `S` is invertible (i.e., there are independent
eigenvectors) we have: `A=SL(S\inv)`.

Note that, naturally, repeated application of a linear transformation
leaves eigenvectors untouched. But the eigenvalues keep getting squared.

Note: `A^k=0` iff all eigenvalues `<1`.

If you want to find `A**k`, you can easily do this as
`A^k=S(L^k)(S\inv)`.

NB: Symmetric matrices have all real eigenvalues and orthogonal
eigenvectors.

The idea behind taking powers is that the system is not linear in `k`,
the power of `A`. So in some sense the system is "evolving" over time.

Note that if you have a differential equation expressed matrix style,
the steady states are those corresponding to the eigenvalue zero.

For differential equations, stability requires the eigenvalues all
have real component `<0`; that is, everything keeps shrinking toward
zero. To have a stable system, then the eigenvalues can be zero (with
zero imaginary component); that would imply that eigenvector is
getting frozen.

## Calculus

Note that `A` is the Jacobian of the linear transformation it defines.

**TODO2**:

* Do all the problems through chapter 6.
* Watched through lecture 19.
* I probably should study row-reduced echelon form. My understanding
  of `LUP` decomposition is good, but rref is a common technique.
* Also: left inverses are also right inverses. Why? I think it has to
  do with the fact that you can see a matrix as begin a linear
  transformation of row vectors if applied to from the right side. I
  think I need to consider duality more deeply...
* Jacobian, Hessian, when is a function "differentiable".
* Should study multivariable calculus. Vector calculus.
* Div, Grad, and Curl?
* What are eigenvalues really?
* https://en.wikipedia.org/wiki/Matrix_decomposition
* Prolly want to take a statistics course. I have trouble with
  Elements of Statistical Learning for this reason.
