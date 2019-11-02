## Euclid's Algorithm

The idea is simple. If `a < b`, and you want to find the greatest common
divisor, it must divide both `a` and `b`. But that means it must divide
`b - a`. Therefore you can just recursively apply to `a, b - a`.

Of course, you might as well recursively apply to `a, b mod a`.

Finally, when `b mod a` is zero, you know that `a` is the GCD.

## Bezout's Identity

There exists `x, y` such that:

    xa + yb = GCD(a, b)

I believe this is called *Bézout's identity*. Let's prove this. First,
let:

    z = GCD(a, b)
    a = k_a z
    b = k_b z

So:

    x k_a z + y k_b z = z

So the question is:

    x k_a + y k_b = 1

Now, assume `k_a < k_b` for a second.

We know that `k_a, k_b` are relatively prime. So consider `k_a` in `Z
mod k_b`. If we consider powers of `k_a`, we know this never becomes
zero, since it will never be divisible by `k_b`. Thus `k_a` is
invertible. Which means there is an `x` such that `x k_a = 1 mod k_b`.
Which means there is a `y k_b` such that `x k_a - y k_b = 1`. So if you
just flip the `y` around, you get the desired number above.

## Extended Euclidean Algorithm

Here's how you do it. Let's say we want to find `x, y` for prime numbers
139, 71.

    139   1   0 (139 = 1*139   +   0*71)
    71    0   1 ( 71 = 0*139   +   1*71)
    68    1  -1 ( 68 = 1*139   +  -1*71)
    3    -1   2 (  3 = -1*139  +   2*71)
    2    23 -45 (  2 = 23*139  + -45*71)
    1   -24  47 (  1 = -24*139 +  47*71)

This basically just keeps track of how to get the smaller and smaller
residual value.

Now, note that the inverse of `71 mod 139` is `47`. That's because if
you subtract `24*139`, you are left with a residual of 1.

This shows you can quickly invert numbers for any modulus.
