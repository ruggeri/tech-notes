## Simon's Algorithm

We have a function `f` defined on `n` bits where for some `a` `f(x) =
f(y)` iff `y = x + a` (bitwise mod n). We want to find `a`.

Note that if we try `k` values of `x`, then maximum we can test `k(k -
1) / 2` values of `a`. Yet `a` is in a domain that grows exponentially
in `n`. If we know nothing about `f` then there is no way to solve
this faster.

Here is an expected polytime quantum algorithm (Simon's algorithm).

First, apply `H^{\otimes n}` to `n` input qubits to achieve a uniform
superposition. Then run those qubits through a circuit that produces
`|x> \otimes |f(x)>`.

We now want to "undo" the `H^{\otimes{n}}` operation by applying it a
second time to the `|x>` values. However, because the `|x>` qubits are
entangled with the `|f(x)>` qubits, applying `H^{\otimes{n}}` to the
`|x>` no longer quite undoes the original Hadamard.

So let's consider a fixed value of `|f(x)>`. Assuming `a != 0`, there
are exactly two values of `|x>` that are associated with `|f(x)>`:
`|j>` and `|j + a>`.

So what happens when we put `|j>` and `|j + a>` through the Hadamard?
So, let's note this fact about Hadamard:

    H^{\otimes n} |j> = \sum_{i = 0}^{2^n - 1} (-1)^{i \cdot_n j}/(\sqrt{2}^n) |i>

(`\cdot_n` means bitwise dot product mod 2. i.e., do `i, j` share an
even number of bit values in common?).

We said that for any value `|f(x)>` there were two states `|x>` and
`|x+a>` that are entangled with that state. So when we apply the
Hadamard, we don't affect `|f(x)>`, but for the other qubits we have
the amplitude for each `j`:

    (-1)^{number of 1s in common to x and j}
    + (-1)^{number of 1s in common to x+a and j}

Interesting. So note that these will constructively interfere when `a`
has an even number of 1 bits in common with `j`. That is, when

    a \cdot_n j mod 2 = 0

It doesn't matter what the input `x` qubits were. When we have `a
\cdot_n j mod 2 = 1`, then we will have destructive
interference. Which means that, no matter what `|f(x)>` is, if we
measure the input qubits at the end, we must measure a value `j` which
has an even number of bits in common with `a`.

## Using our Quantum Circuit as Part of a Procedure

By sampling `j`, we have halved our search space for `a`. Only 50% of
`a` values will have an even number of 1s in common with `j`.

One way to think about things is this. Each sampled `j` is *orthogonal*
to `a` in the mod 2 world. After you sample `n-1` linearly independent
vectors, you are left with a subspace of dimension 1 that is orthogonal
to all of these. You can find this by standard solving of linear
equations.

It's a little different than sampling in `R^n` (where `n` randomly
chosen unit vectors are linearly independent with probability 1.0), but
you should get a set of `n-1` linearly independent vectors in `O(n)`
samples.

## Comments

A typical trick here is to put all inputs into superposition using
Hadamard product. We then use entanglement to isolate cases that are of
interest to us (those with the same `f(x)=f(x+a)` value).

Here we get an exponential speedup by using a quantum computer, but the
problem is of course pretty abstract. But it's a start!
