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
to depend on the *other* members of the new basis, and how much `e_i'`
is in `v` can be different for different completions of the new basis!

So we see we can't make every basis happy. But we can make *some*
bases happy. So what then is the natural way to extend this notion of
decomposition?

We can bring in geometry. I *declare* that the original basis `B`
consists of unit length vectors, all at right angles. This is called
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
