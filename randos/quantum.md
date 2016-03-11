## Classical Computation

In a classical computer, `n` bits can take on `2**n` different
values. Therefore, a setting of a classical computer implies a *unary*
representation of length `2**n`. For instance `011` implies `(0, 0, 0,
1, 0, 0, 0, 0, 0)`.

Operations of the machine can be representated as matrices of
dimension `2**n` where the rows are themselves unary vectors.

## Classical Probabilistic Computation

As a generalization, imagine a machine where the bits take on values
between `0` and `1`. For instance:

    (0.75, 0.25) => (0.1875, 0.0625, 0.5625, 0.1875)

Note that this representation is a unit vector in the L1 norm.

Operations of the machine can be represented as real-valued matrices
of dimension `2**n` preserving the L1 norm. These are sometimes called
*stochastic matrices*.

## Quantum Computation

In a quantum machine, the probability distribution over the state
vector is complex valued:

    (i/4, -i/4, 1/4, -1/4)

When we "collapse" this, we draw from a probability distribution
corresponding to the norms of the components. Thus, the complex vector
must be a unit vector with respect to the L2 norm.

Transformations on the machine correspond to matrices preserving L2
norm. These are called *unitary matrices*.

## Random Thought

We think it's weird that "observation" determines reality. But
observation is *not* subjective; it involves a process of
*measurement*, which necessarily involves *constraint*.
