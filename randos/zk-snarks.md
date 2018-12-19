## Homomorphic Encryption

Homomorphic encryption means that an arbitrary computation C can be
performed on an encrypted value `E(x)`, and the result is
`E(C(x))`. Fully-homomorphic encryption is invented by Gentry.

Here are some partially homomorphic systems (uselss, but still). For
RSA, an encrypted value `x` has the form `x**e`. Say you want to
encrypt the value `x**a`. Then simply take `(x**e)**a`. This is of
course `(x**a)**e`.

In fact, more generally, we can say that for any `x, y`, you can
compute `x**e y**e = (xy)**e`.

## Homomorphism wrt Polynomials

Consider the system `g^x mod p`, where `g` is a generator for
`Z_p`. The discrete log problem is supposed to be hard: it's supposed
to be hard to go `g^x => x`.

Now, they give an example of how Bob can ask Alice to perform a "blind
evaluation" of Bob's secret value `s` into Alice's polynomial,
computing `P(s)`. Basically: Alice never learns Bob's `s`, and Bob
never learns Alice's `P`.

To do this, Bob submits to Alice the series of values `g^1, g^s,
g^{s^2}, g^{s^3}, ...`. Notice that this exposes `g`, but then `g` was
never a secret parameter! What it does *not* expose is `s`.

Alice wants to respond with `g^{P(s)}`.

Now, we know that `P(s) = a_0 g^0 + a_1 g^1 + a_2 g^2, ...`. Therefore,
we have

    g^{P(s)} = (g^{a_0}) (g^{a_1 s}) (g^{a_2 s^2}) (g^{a_3 s^3}) ...

Alice can calculate this. She uses Bob's values like this:

    g^{s^0} => (g^{s^0})^{a_0}
    g^{s^1} => (g^{s^1})^{a_1}
    g^{s^2} => (g^{s^2})^{a_2}
    ...

She can then multiply all these together to produce the answer. Bob
now has the hidden version of `P(s)`; namely `g^{P(s)}`.

Note two things:

1. Bob doesn't know what `P(s)` is though,
2. Bob can't be sure that Alice has calculated this properly.

## Verifying Alice's Work

The trick is to submit *two* versions of the `s` value. First, Bob
submits:

    g^{s^0}, g^{s^1}, g^{s^2}, ...

But for some secret `alpha`, Bob also submits:

    g^{alpha s^0}, g^{alpha s^1}, g^{alpha s^2}, ...

Of course, Alice cannot determine `alpha` from the information that
Bob provides.

Alice then calculates `g^{P(s)}` by taking the appropriate linear
combination of the submitted powers of `g`. But she *also* does the
same with the powers of the form `g^{alpha s^i}`. Effectively, she is
computing both:

    g^{P(s)} and g^{alpha P(s)}

Okay. Then note that Bob can easily check that `g^{alpha P(s)} =
g^{P(s)}^alpha`.

Now we know that Alice has correctly computed the desired
answer. However, **what is the use of all this**? Sure, Alice has
performed some computation for some polynomial: but why does Bob care?
Bob didn't get to *pick* the polynomial...

## Encoding Computations as Quadratic Arithmetic Program

I will show you that any computation can be "encoded" as a
polynomial. The language of computation we have are *arithmetic
circuits*. They are basically bounded computation (without loops) that
lets you add/multiply/subtract/divide all `mod n`. This model is as
powerful as any bounded language.

Now, this arithmetic circuit can further be encoded as a *quadratic
arithmetic program*. This is the first step toward a polynomial. Here
is how.

First, consider if the network *only* has multiplication. Then you
have inputs `(x_0, x_1, ..., x_n)`. But then, for each multiplication
gate, you have some inputs and some outputs. These are `(x_{n+1},
x_{b+2}, ..., x_{n+m})`. The final output is of course `x_{n+m}`.

Now consider any gate `k`. Let us say that `k` consists of multiplying
`x_i` and `x_j`. Then I say that we must have this constraint:

    (e_i \cdot x) * (e_j \cdot x) = (e_{n + k} \cdot x)

(here `e_i` is unit vector for `i`, and we assume `x` is a column)

See how these are just selecting out the right values, multiplying
them, and making sure they equal the output for the given gate?

So the overall computation is defined by a *matrix* A such that:

    (Ax) coordinatewise_multiplication (Bx) = Cx

Here, rows of `A` and `B` represent left and right inputs to a
multiplication, while rows of `C` (which have one value each) specify
outputs of units in the arithmetic circuit.

`A, B, C` specify the circuit. Knowing `x` is equivalent to having
performed the computation.

Note: since the popcount in a row of `A` and `B` need not be one, we
can multiply together linear combos of prior outputs. Also, if we have
one input wire fixed to `1`, we can do sums.

## QAP to Polynomial

They are going to transform this from a problem about an arithmetic to
one about polynomials.

Now, let us consider that `A, B, C` are all of dimension `m x
(n+m)`. That's because the computation involves `n+m` input/output
values, and we need to verify that the `m` outputs were calculated
correctly.

Let's encode `A` as `n+m` polynomials of degree `m-1`. What we do is
this: for each column `j`, we use *Lagrange interpolation* (covered in
Shamir secret sharing) to compute a degree `m-1` polynomial such that:

    A_{i, j} = P_j(i)

For each `j`, this is `m` constraints, which determines a degree `m-1`
polynomial (remember, two points determines a line; and a line is
parameterized by two values).

We do the same for `B`, `C`. For each of three matrices, we have `n+m`
polynomials, each of degree `m`.

We have now encoded the QAP as a bunch of polynomials. But how does
this help us?

## Checking the Polynomials

Let's call the polynomials we've found:

    P_{a, j}, P_{b, j}, P_{c, j}

Now, we know that there is one `j` value per `n+m` columns. So if we
have a proposed solution `x` of dim `n+m`, we can take the weighted
sum of:

    \Sum_j x_j P_{a, j} * x_j P_{b, j} - x_j P_{c, j}

This is *itself* a degree `m-1` polynomial.

**TODO**: TO BE CONTINUED...

Sources

* Buterin's QAP tutorial is very good:
    * https://medium.com/@VitalikButerin/quadratic-arithmetic-programs-from-zero-to-hero-f6d558cea649
* The ZCash folks have a good development of zk-SNARKs too.
