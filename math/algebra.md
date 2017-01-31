These notes are derived from Strang's Introduction to Linear Algebra.

## Ch1: Intro to Vectors

Linear algebra is about linear combinations of vectors. We know how to
scale and add vectors. If we have two vectors represented in terms of
a common basis, then it is easy to calculate the representation of
their sum.

**Dot Product Derivation**

The dot product is introduced. Let's start with the idea of an inner
product.

An inner product is used to decompose a vector into a linear
combination of basis vectors. Given a basis, there is a natural inner
product where `<v, e_i>` is exactly how much of `e_i` is used in the
linear combination of basis vectors that sums to `v`. If `v` is
already *represented* as a linear combination of `B` (for instance, as
a "row vector"), then this simply "picks out" the value of the `i`-th
coordinate.

So a choice of basis gave us this "kernel" of an inner product. Right
now, we can only project onto basis vectors, but we will want to
extend our inner product so that we can project onto any vector.

Already, we have some obvious properties of our inner product:

    <av, e_i> = a<v, e_i>
    <v + w, e_i> = <v, e_i> + <w, e_i>

Ideally, we would like it that `<v, e_i'>` always decomposes `v` into
a linear combination of any `e_i'`. However, there is no way to make
this work consistently. The reason is perhaps clear: `<v, e_i'>` has
to depend on the *other* members of the new basis, but how much `e_i'`
is in `v` can be different for different completions of the new basis!

So we see we can't make every basis happy. But we can make *some*
bases happy. So what then is the natural way to extend this notion of
decomposition?

We can bring in geometry. I *declare* that the original basis `B`
consists of unit length vectors, all at write angles. This is called
an *orthonormal* basis. Because I've chosen a Euclidean geometry, we
can talk about the *length* (or *norm*) of any vector `v`. This is:

    sqrt(Sum <v, e_i>^2)

That lets us define a circle/sphere/hypersphere of unit vectors. We
define a number `pi` such that the circumference of the unit circle is
`2pi`. We now define a measure of angle, called *radians*. The
definition of angle works like this: shrink two vectors to unit
vectors. Now, consider them inscribed in the circle. How long is the
arc on the circle between their endpoints?

The notion of radians gives rise in turn to `sin` and `cos`, which
define how to decompoes a unit vector into `e_1` and `e_2` when given
the angle of `v` formed with the unit vector `e_1`.

Great, now we've brought in all these geometry notions. We don't
really need these, except that it is now going to suggest a natural
way to extend the inner product.

The idea is this, we're going to say that `<u, v>` is the amount of
`v` needed in the linear combination that forms `u`, assuming that the
basis consists entirely of orthogonal vectors. This happens exactly
when the new basis is a combination of *rotation* of the old basis,
along with some permutation of the axes.

It's great to say we want to extend the inner product this way, but
how does that work? Well, consider `e_1' = cos(theta) e_1 + sin(theta
e_2)` and `e_2 = -sin(theta) e_1 + cos(theta)e_2`.

Now, how can we decompose a vector `v` into `e_1', e_2'`? It suffices
to project `e_1` and `e_2` onto these vectors. Now, by definition of
`e_1'` and `e_2'`, we know how to project these *onto* `e_1` and
`e_2`, but now we want to do the opposite. Luckily, this is easy! The
dot product has to be *symmetric*. I will leave this hand-wavy because
I'm lazy today (this is actually an easy proof), but the idea is if
`e_1'` is a rotation of `e_1` by `theta` degrees, than `e_1` is a
rotation of `e_1'` by `-theta` degrees! `cos` is symmetric about `0`
so this shows that the inner product must be symmetric, too!

So since `<e_1, e_1'>=<e_1', e_1>`, we know since `e_1'` is a rotation
of `e_1` by `theta` radians, this is `cos(theta)`. Likewise `e_2`
projected onto `e_1` is `sin(theta)`.

Now, we can do some expantion:

    v = ae_1 + be_2
    => <v, e_1> = <ae_1 + be_2, e_1'>
    => a<e_1, e_1'> + b<e_2, e_1'>
    => a cos(theta) + b sin(theta)

Note that `v=(a,b), e_1'=(cos(theta), sin(theta))` when represented in
the original basis. So we can easily calcualte the inner product as a
**dot product** by using the representations!

Note that the dot product is sensitive to scaling of the basis. So
when you use it to decompose a vector, you need to normalize the basis
elements to length 1.

From this definition, it is clear that `cos(theta) = (u \cdot
v)/(|u||v|)`. The idea is this. First, shrink `u` and `v` so that they
are inscribed in the unit circle. Now, spin this around so that `u`
aligns with a coordinate axis. By definition of how we defined the
inner product as an extension, this is invariant under rotations. So
last, the amount of `u` projected onto `v` is by definition the `cos`
of the angle formed between them!

A note: the inner product defined by a basis is not unique to that
basis. Any rotation of the basis, or reflection of the basis, would
generate the same inner product.

**Properties of the Dot Product**

Whew! That was a lot! Properties of the dot product we got from the
preceding section:

1. We saw how to normalize a vector by dividing by its norm.
2. The dot product is zero when vectors are orthogonal.
3. The dot product of unit vectors always has magnitude <= 1.
4. Cosine formula: the dot product of unit vectors is equal to the
   cosine of the angle between.

**Linear Transformations/Matrices**

**Matrices** map linear combinations to linear combinations. Such a map is
called a **linear transformation**. The first column is what the first
basis vector maps to, the second basis vector maps to the second
column, etc.

The product of a matrix with a vector is the result of applying the
transformation to the vector. I like to see this as a weighted sum of
the columns.

We can *compose* transformations. This involves matrix
multiplication. It is quite simple. Given `BA`, take the first column
of `A`; that is what `e_1` maps to under `A`. Then apply `B` to this
column. This is now equal to what ought to be the first column of the
product `AB`. This suggestions a method of calculation.

Note to self: matrix multiplication involves `O(n**3)` time.

Another common way to apply a matrix to a vector is this: take the dot
product of each row in the matrix with the vector. **TODO**: what is
the intuition behind this?

**Inverting A Matrix**

How do we "undo" a linear transformation? That is, given a vector `v`
in terms of `B`, how do we find the vector `u` such that `v=Au`?

This is easy if we decompose `v` into a linear combination of the
columns of `A`. If the columns of `A` are orthogonal in the geometry
implied by `B`, we can do this by using the dot product. If, as is
typical, `B` is not orthogonal, then we must do more work.

So let's take a moment to consider a matrix that maps the basis to
another orthonormal basis. To invert an element `v`, we need to
decompose it into columns of `A`. But since the columns are
orthonormal, we can use the dot product just as we said. So `e_1`
should map to a vector which is `(<e_1, A_1>, <e_1, A_2>, ...)`; that
is, we project `e_1` onto each column of `A`. Of course, this is just
the first row of `A`.

By definition of how a linear transformation is represented, we write
this vector of projects as the first column. Thus, the first row has
become the first column.

This demonstrates that any orthogonal matrix (which represents
rotations + flips) always has its inverse equal to its transpose! Thus
we write:

    Q=Q\trans

Since, `Q` is often used for an orthogonal matrix.

## Ch2: Solving Linear Equations

A system of linear equations can be seen as a matrix. Each equation is
a coordinate in the new space. Each equation defines a row in the
matrix. It's how much each coordinate in the original space
contributes to the value of the second space.

If we view the problem like this, then the solution is to take the
target in the transformed space and find its inverse under the
transformation.

The typical way to find the solution is to perform **Gaussian
Elimination**. What this does is slowly multiply each side of `Ax=b`
by a set of basic transformations until `A` is eliminated and `b` has
been transformed to its inverse under `A`.

The operations are these:

1. Scale a row.
2. Add or subtract a multiple of a row from another.
3. Swap rows.

The algorithm is like this:

1. Start with the first row. Scale so its initial entry is `1`.
    * (For each transformation to `A`, perform the same transformation
      to `b`).
2. Subtract the appropriate amount from the other rows so that you
   eliminate the other entries in the first column.
3. Move to the second row. Repeat the process: scale so that the
   second element in the second row is 1, and then subtract as needed
   from the other columns.
4. The only problem is if you encounter the `i`th row and it has `0`
   at the `i`th column. In that case, swap two rows.

If you apply the transformations to `b`, you will end up with convert
it to its inverse. You may instead apply these transformations to `I`,
which will convert this into the inverse linear transformation.

If you encounter a column of all zeros, then we've shown that the
**column rank** of this matrix is not **full**. That means that one of
the basis vectors is mapped to a vector that can be written as a
linear combination of the images of the other basis vectors. That is:
the columns are not all linearly independent.

Geometrically, what we are doing is this. We are saying: I want `e_1`
to only impact the first coordinate of `b`; I want to eliminate its
effect on the other coordinates. Then we say: good, now I want e_2 to
only involve itself with the second coordinate in the image space.

Note: the version where we transform `I` can be seen as finding the
inverse for *n* vectors (the columns) simultaneously. That's kind of
interesting: to see it as nothing more than a parallelized/vectorized
version of solving for one inverse.

**TODO**: I should have more geometric intuition about manipulations
  of the rowspace.
