## Adding and scaling complex numbers

So, what is a complex number? A complex number has the form `a +
ib`. You could also write this `(a, b)`, so this sure looks like some
kind of vector in the cartesian plane. But when we talk about the
complex numbers, we're also talking about the standard *operations*,
which are addition, scaling, and exponentiation. Our goal is to extend
these concepts from the real numbers. That is, they should agree when
we are using complex numbers `(a, 0)`.

Now, we know that any vector in `R^2` can be written in *polar
form*. Basically, you know that you can always normalize a 2d vector
to length one, and this lies on the unit circle. Any point on the unit
circle can be written as `cos(theta)e_1 + sin(theta)e_2`, because
that's exactly how we define `cos` and `sin`.

Thus, likewise, we have that any complex number can be written in the
form `r(cos(theta) + sin(theta)i)`.

The complex number can be interpreted as a vector; the complex number
is basically just that vector represented with respect to some
basis. But of course, the basis is an arbitrary choice; the same
vector could be represented in many bases. For instance if `v` is
represented as `(a, b)` in the basis `(e_1, e_2)` then note that `v`
is represented as `(b, a)` in the swapped basis `(e_2,
e_1)`.

Here is a principle: when defining an operation `f(c_1, c_2)` we would
like that this is equal to `swap(f(swap(c_1), swap(c_2)))`. If that
were not true, the function `f` would do different things to the same
vector `v` when fed different representations of that same vector in
different bases.

It is clear, then, that we must extend addition and scaling by a
*real-valued* constant from the traditional real definition to a
coordinatewise definition for complex numbers.

## Multiplying complex numbers

This brings us to the next question: how should we define scaling *by
a complex number*. That is: how do we multiply two complex numbers?

Well, let's consider if you tried to multiply `i * i`. Per our above
statement, we ought to be able to swap `i`s for `1`s, do the
operation, and then swap back. This says that `i * i` should equal
`1 * 1`, but then swapped to `i`. But that can't be right! What, is
`i` supposed to be an identity for itself?

Let's illustrate the problem. Consider multiplying `(1, 0)` by `(1,
0)`. This of course must be `(1, 0)` again, because we want to
preserve the action of multiplication on the reals.

Now, imagine if I switch bases on you. I'm going to shrink the basis
vectors by `1/2`, so that the old `(1, 0)` is the new `(2, 0)`. Now, I
do the same multiplication by the original vectors: `(2, 0) * (2, 0)`
which gives me `(4, 0)` in the new basis. Even after I translate back
to the old basis, I get `(2, 0)`, not `(1, 0)`.

The problem is that when I multiply `z_1 * z_2`, you can think of
`z_1` as a transformation being applied to `z_2`. This transformation
is described in terms that are *independent* of the choice of
basis. For instance: multiplying by `(2, 0)` means that, whatever your
basis vectors are, scale them each by a factor of 2.

Let us return to the original question. We want `i * i`, which is `(0,
1) * (0, 1)`. I say: let's do this with the swapped basis, then swap
back. So I try `(0, 1) * (1, 0)`. Now, you might say "I don't know how
to scale a constant by `i`!" But, if we want to preserve symmetry,
then multiplying by a constant on *either* side ought to be the same,
so we should get `i` again.

So, in the new basis, who is `i`? It depends on what the second, new
basis vector is, and that could literally be anyone! So we haven't
gotten anywhere!

## Insist `i^2 = -1`

Okay, it looks like I failed. So let's start with the whole point of
introducing `i`. We want `i` to be that operation that, when you apply
it twice, it's the same as applying the `-1` operation.

Now, `alpha * i` for `alpha` a real number should be scaling plus that
"rotation" operation.

Let's consider arbitrary `z = a + bi`. I'd rather write this in polar
form: `r(cos(theta) + sin(theta)i`. I know how to scale, so I only
need to figure out how to multiply by `cos(theta) + sin(theta)i`.

Now, we want our operations to be *linear*: `(f + g)(v) = f(v) + g(v)`
AND `f(v + w) = f(v) + f(w)`. That implies the distributive law. That
implies that:

    (cos(theta_1) + sin(theta_1)i) * (cos(theta_2) + sin(theta_2)i)
    = [cos(theta_1)cos(theta_2) - sin(theta_1)sin(theta_2)]
    + [sin(theta_1)cos(theta_2) + sin(theta_2)cos(theta_1)]i
    = cos(theta_1 + theta_2) + sin(theta_1 + theta_2)i

Okay. Wonderful. This shows that applying a multiplication by `z` is
equivalent to rotation by `theta` and scaling by `r`.

This basically says: `i` is the unique linear operation that, when
performed twice in a row, maps the identity to `-1`.

## Exponentiation

Phew! This was hard! The question in extending exponentiation is this:
what defines exponentiation? You can define exponentiation recursively
for non-negative integers, but this doesn't tell you what to do for
negative exponents. However, we seem to like this idea that
`e^{a+b}=(e^a)(e^b)`. Now, by our recursive definition, this must be
true for `a,b` non-negative numbers. Nobody says, though, that this
formula *has* to work for negative integers, even though that by now
seems intuitive. Maybe it's brainwashing, though.

Likewise, why should `e^{ab}=e^a^b`. It has to be true for `a,b`
integers, but again, it doesn't have to be true for rational
numbers. But again, we seem to like that algebraic property.

Underlying these principles is a more fundamental idea. It is this:
that the derivative of `e^t` is itself `e^t`. That requires *both*
rules to be true.

Once again, this rule will teach us how we may extend to complex
exponentiation. Consider `e^it`. When `t = 0 `, then this is `1.0` and
the derivative is `i`, presuming the exponentiation rule still holds.

Now, this is exactly the scenario of a rotating body: the velocity is
perpindicular to the radius connecting the axis of rotation to the
body. For that reason, we can immediately guess that `e^it` should be
set to `cos(t) + sin(t)i`. In exactly this case, when we integrate
out, we will recover the proper formula:

    \Int_0^t i(cos(t) + sin(t)i) dt = cos(t) + sin(t)i

This is satisfying. Note that:

    e^{it}^2 = e^{i(2t)} = cos(2t) + sin(2t)i

That makes sense, as multiplying by a unit complex number adds
angles. In this case, squaring a unit complex number *doubles* the
angle. That is exactly what you expect!

Of course, we want to talk about all complex numbers: `e^z`. So:

    e^{a + bi} = (e^a)(e^bi) = (e^a)(cos(b) + sin(b)i)

Here we see that this equation just means: the unit real vector with
norm `e^a` rotated by `b` radians.

## Extending our result

Let's think about `e^t`. We can take the derivative which is also
`e^t`. This means: the velocity should constantly be increasing, since
the magnitude of this value grows with `t`.

Next, we can consider `e^it`. This has derivative `i e^it`. This says,
the velocity should constantly be changing direction, since it is
perpindicular to the position. Also, because the position is always of
distance 1 from the origin, this travels in a circle, and does not
increase the magnitude of the velocity, but only the direction of that
velocity.

We can combine these two factors, like so. Consider `e^{1 + it}`. In
that case, the initial position starts out further from the
origin. The derivative is now `i * e^{1 + it}`. That is now a constant
speed of `e`. That's exactly what we need to complete the circuit in
`2pi` "seconds".

We can next ask about `e^{ikt}`. This now has derivative
`i*k*e^{ikt}`. As ever, this is directed perpindicular to the
radius. This increases the speed by a constant factor so that 1 second
covers more radians. The circuit is completed in `1/k`th the time.

We can next ask about `e^{r(cos(theta) + sin(theta)i)t}`. We've
previously considered when `theta = pi/2`. That told us the velocity
ought to be perpindicular to the position. Now we have a new
derivative:

    r(cos(theta) + sin(theta)i) * e^{...}

If `theta` is not `pi/2` or `-pi/2`, this will no longer describe a
circle. Now, in the case that `theta` is not zero, there is a rotation
factor. Because this is still proportional to the position, then the
rotation should occur in `2pi / (r * sin(theta))` seconds.

However, there is now an increasing scaling factor. The length of the
radius is now `e^{r(cos(theta))t}`, because it is being stretched
straight out by a constant factor of `r cos(theta)`.

I'd like to mention that we've currently covered at this point:

    (rcos(theta) + sin(theta)i)^t

because this is equal to

    e^{ln(r)t + theta*t*i}

Effectively this says "apply de Moivre's rule" (which says exponent of
complex unit vector multiplies angle) and do a little stretching.

Let's bring this on home. We want to consider `z_1^{z_2 * t}` for
arbitrary complex numbers. Per the above, this really means:

    e^{[r_1(cos(theta_1) + sin(theta_1)i)] *
       [r_2(cos(theta_2) + sin(theta_2)i)] t}

This basically just means you combine the stretching factors and the
rotation; it's just what we saw before.

Basically, if you have in the base `r(cos(theta) + sin(theta)i)`, that
is equivalent to having `ln(r)t + theta*t*i` in the exponent.

I'm a little tired so I'm going to forgo further consideration of this
topic. I think by now I have a great intuition of the typical exponent
I need which is `e^{theta*i}`.

**TODO**: haven't considered adding a fixed `i` term to the exponent.
