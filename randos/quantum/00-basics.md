**Classical Computing**

In classical computing, we have deterministic machines operating on
bits. Bits have a value which is zero or one. If the machine has `k`
bits of memory, then its current state is in the space `2^k` which is
`k`-dimensional.

**Probabilistic Computing**

One way to conceive a probabilistic algorithm is to say that it has
access to `k` bits of memory, randomly initialized. You don't have to
say that it has probabilistic operation: the operations are still
deterministic. Remember: any "probabilistic operation" can really be
seen as a deterministic operation controlled by an if switch that uses
one randomized input bit.

A not particularly useful way to look at a probabilistic machine's state
is as an L1 unit vector in `R^{2^k}`. That is: it must sum to one. The
kind of matrix that preserves L1 norm is called a *stochastic matrix*.
The reason I say not very useful is that I see no reason to keep track
of the probabilities throughout the computation. You do just as well to
sample right at the beginning, and then run a deterministic machine.

We might as well just "collapse" the probabilistic state and then
proceed with deterministic operations. One reason to do this is because
tracking the probabilistic state requires storing a vector of `2^k` real
numbers. Note that the advantage is that on collapse, you have a
deterministic state in the space `2^k`

Note that randomized algorithms broaden the notion of efficiently
computable from P to BPP (polytime to achieve any accuracy level). There
is also ZPP, which is expected polytime to achieve 100% accuracy (these
are sometimes called Las Vegas algorithms). ZPP is the intersection of
RP (polytime, always rejects if false, but accepts with prob 2/3 if
true) and co-RP. You get ZPP by just alternating RP and co-RP until you
get a definitive answer.

For a very long time, we didn't have an algorithm for primality testing
in P. But a simple BPP approach is called *Fermat testing*: you know
that `x^p = x (mod p)`, for any `x`. So keep randomly choosing `x`s, and
if you never find a violation, then this is probably a prime! (Important
note: if `p` is not actually prime, there are at least `sqrt(p)`
non-coprime numbers `x<p`, so you should find one after not too long of
testing.)

Anyway, this *suggests* that BPP might be larger than `P`, but we don't
strictly know.

**Quantum State**

From above, I noted that probabilistic state is not necessarily very
interesting: you should collapse the L1 unit vector in `R^{2^k}`
immediately then do simple deterministic computation.

In quantum, things are different. It is not at all the same to collapse
the quantum state at the beginning and then perform deterministic
operations.

Each *qubit* of quantum state has a state of the form:

    (\alpha, \beta) where |\alpha|^2 + |\beta|^2 = 1

That is, the qubit state is a unit vector in `C^2`.

To indicate the *basis* vectors `e_0`, `e_1` (corresponding to zero and
one values of the qubit), we write  `|0>, |1>`, which is called *Dirac
notation*. Thus the quantum state can be written as:

    \alpha |0> + \beta |1>

You can **observe** or **measure** a qubit's value. It will become
either `|0>` or `|1>` with probability equal to `|alpha|^2` or
`|beta|^2`. Note that this means there are an infinite number of ways to
have a 50/50 probability distribution over a qubit. Here are sixteen of
them: `\alpha` can be `1/\sqrt{2}`, positive or negative, complex or
not, and the same applies for `\beta`.

The `\alpha, \beta` are called probability *amplitudes*. Why deal with
these amplitudes at all? Why not just use the L1 representation?

**Unitary Transformations**

The reason is that quantum state evolves through unitary transformations
of `C^2`, not just stochastic transformations of `R^2`. Unitary
transformation just means a norm preserving invertible function. Real
unitary transformations are the orthogonal matrices. Unitary
transformations basically spin you right round baby; they are all
rotations (and flips).

One thing to note is that since all transformations are unitary, they
are invertible, which means computing is reversible. That's
interesting...

(Note that the inverse of a unitary transformation is its conjugate
transpose. It's just like how an orthogonal matrix has its transpose as
its inverse.)

It is the fact that amplitudes can be *negative* that makes things
different from probabilistic computing. For instance: you can apply a
45deg counter-clockwise rotation to `|0>` to get `(1/\sqrt(2)) (|0> +
|1>)`, and then apply the same operation again to get `|1>` back. You've
applied a "randomizing" operation twice but gotten back something
deterministic!

Note how everything would be different if you had observed after the
first rotation. You would have gotten `|0>` or `|1>` with equal
probability, and then your state would have evolved to `(1/\sqrt(2))
(|0> + |1>)` or `(1/\sqrt(2)) (-|0> + |1>)` with equal probability.

If we observe, these states look the same; the signs don't matter. It's
only the fact that by *not* observing we get the linear combination of
the two where the signs *do* matter.

Aaronson says that we don't need complex values for quantum computing;
he says that negative amplitudes are all we need.
