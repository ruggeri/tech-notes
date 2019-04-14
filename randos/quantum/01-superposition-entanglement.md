## Multi Qubit Systems

We can have systems with multiple qubits. These systems are formed by
the *Kronecker product* of `C^2`. This is a little different than the
normal "tensor product." I believe the tensor product `(1, 0) \tprod (0,
1)` merely *glues together* vectors. This would mean that a three qubit
system would be described by six dimensions. That is **not correct**.

A three qubit system is described by an 8 dimensional space. There is
one amplitude for each bit string `000, 001, 010, 011, 100, 101,
111`. These are the basis states. We often write `|101>`. We might
also write `|5>_3` to mean `|101>`. The subscript 3 here is indicating
that this is in the 3-qubit space.

Note: while a `k` classical bit system's state is in a `k` dimensional
space, a `k` quantum bit system's state is in a `2**k` dimensional
space.

We've already seen a *superposition* of basis states, it's just:

  \sum \alpha_i |i>_k (for `k` qubits, `i` ranges from `0` to `2^k`)

Note: a multi-qubit system is in a basis state only if every qubit
individually is in a basis state.

## Entanglement

We say that a multi-qubit system state is **decomposable** or
**separable** if it can be written as a tensor product of smaller
qubit systems. We say it is a **product state** if it can be written
as a tensor product of 1 qubit systems. Otherwise, we call it
**entangled**. Here is your classic product state:

    (\alpha_1 |0> + \beta_1 |1>) \tensor (\alpha_2 |0> + \beta_2 |1>)
    =
    \alpha1\alpha2 |00> + \alpha1\beta2 |01>
    + \beta1\alpha2 |10> + \beta1\beta2 |11>

Here is your classic entangled state:

    1/sqrt{2} |00> + 1/sqrt{2} |11>

A pair of qubits in this state is called an *EPR pair*.
