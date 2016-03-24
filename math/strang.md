```
var v = document.querySelector('video'); v.playbackRate = 2.5
```

## Solving system of equations

Linear systems of equations are always like `Ax=b`:

```
a_11 x_1 + a_12 x_2 = b_1
a_21 x_1 + a_22 x_2 = b_2
...
```

So basically, you can see `A` as a linear transformation. To solve
this, we want to apply `A\inv` to `b`.

## Inverting a matrix

To invert a matrix, let us first decompose it into three
easy-to-invert matrices using Gaussian elimination.

Gaussian elimination allows us to try to restructure the matrix in a
way that makes it easier to work with. Basically, we take the first
column and subtract it from each of the subsequent columns so only the
first column has any non-zero component in the first coordinate. We
then repeat so that only the first and second have any non-zero
component in the first two coordinates. Etc.

This gives us a lower triangular matrix. Corresponding to this is the
upper triangular matrix that gives `A=LU`. The upper triangular matrix
compensates for the column operations we perform to produce the lower
triangular matrix. This is the *LU decomposition*.

This ignores that `A_11` could be zero. In that case, you need to
*exchange* columns. This corresponds to a pivoting of the columns,
which gives us a `LUP` decomposition. If you get to a point where all
the columns have a zero in a coordinate, then the matrix is singular.

BTW: Matlab or a CAS will do row-exchanges for the sake of numerical
accuracy. For instance, small pivots are not desirable.

It is trivial to invert a triangular matrix like this. Therefore
`A\inv=P\invU\invL\inv`.

Elimination is `O(n**3)`. That's because each of `n` columns involves
elimination on `i` other columns (average of `n/2` columns), each of
which has `n` elements. So overall we're talking `O(n**3)`.

## Meaning of LU decomposition

We're breaking up a matrix into two steps: first, make changes to each
basis vector, adjusting only in previous components, then make
adjustments to these, changing only later components.

The point is that by seeing the linear transformation of the basis
vectors in these two steps, we can easily invert it.

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

## Augmented matrix

There is a trick where, instead of producing `U` as you produce `L`,
you instead apply `L\inv` to `I` while producing `L`, then, by doing
*back-substitution*, you apply `U\inv` to `L\inv`. This is done with
the *augmented matrix* `[A|I]`. If you just want to invert one vector,
you can use the augmented matrix `[A|b]`.

I don't cover these because I think doing the `LUP` decomposition and
then inverting is fine.

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

## Orthogonality

**I think some of this is wrong and superseded by inner-product.md**

I have this concept of independence: a vector is a linear combination
of vectors, or it is not. This is a binary notion. Wrt one vector, a
second vector is either a scalar multiple or it is not. What if I want
to generalize this notion: what if two vectors are really similar, but
not quite the same? This is the notion of an *inner product*.

Say I fix a basis, and choose an inner product that identifies pairs
of these vectors as *orthogonal*, having zero inner product. I could
add other requirements: e.g., `<x, y> = <y, x>`. These seems very
natural.

More generally, let us speak of "continuity". Say that we take basis
vectors `b1`, `b2`. Then let `x = (alpha b2) + (1 - alpha)b1`. Then we
want:

```
<x, b2> = alpha
```

We get this if the inner product is *linear*:

```
<(alpha b2) + (1-alpha)b1, b2>
= <alpha b2, b2> + <(1-alpha)b1, b2>
= alpha<b2, b2> + (1-alpha)<b1, b2>
= alpha*1 + (1-alpha)*0
```

I won't fill in all the details, but getting this kind of continuity
implies linearity of the inner product. It also means that we can
*decompose* a vector into components via projection. But it all comes
from us wanting a natural generalization of similarity beyond the
binary notion of dependence.

I won't do the math, but for coordinate vectors, the dot product is
the entirely natural choice. Basically, if you have a basis, call that
orthogonal. Then the linearity requirement of the inner product
implies the dot product.

I always thought the dot product was magic. In fact, it is the natural
choice if we're *looking* for a notion to generalize similarity. We
find dot product by wanting to say vectors aren't just "the same" or
"different" but *how* different.

## Orthogonality and inverses

Since the projection of a vector `v` onto an orthonormal basis gives
you the amount of each basis vector you need to form the `v`, taking
the inner product of `v` with each column of an orthogonal matrix
gives you the inverse of `v` under that transformation.

This is in fact what happens when you take `(A\trans)v`. So (again,
assuming `A` is orthonormal), you have that `A\inv=A\trans`.

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
