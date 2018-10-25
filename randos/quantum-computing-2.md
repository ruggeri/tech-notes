## Basics

The state of a **qubit** is a value in `C^2`. The state is of the form:

    \alpha |0> + \beta |1>
    where
    |alpha^2| + |beta^2| = 1

Here `|0>, |1>` mean `e_0, e_1`: two unit basis vectors. This is the
braket notation.Basically, the state of a qubit is a unit vector in
`C^2`.

Sometimes you'll see `<0|` which means the conjugate transpose of
`|0>`. So `<0||0> = 1`. We simplify and just write `<0|0>` (no double
bar) to mean the inner product of `|0>` with itself.

You can **observe** or **measure** a qubit's value. It will become
either `|0>` or `|1>` with probability equal to `|alpha^2|` or
`|beta^2|`. Note that this means there are many ways to hvae a 50/50
chance (sixteen of them; `\alpha` can be $1/\sqrt{2}$, positive or
negative, complex or not; same for `\beta`).

The `\alpha, \beta` are called **probability amplitudes**.

Qubit state changes through **unitary transformation** and
observation. Unitary transformation just means a norm preserving
invertable function. Real unitary transformations are the orthogonal
matrices. Unitary transformations basically spin you right round baby.

Note that the inverse of a unitary transformation is its conjugate
transpose. It's just like how an orthogonal matrix has its transpose
as its inverse. But now we're throwing in some complex numbers, where
`i * -i = 1`.

What unitary transformations can be performed in hardware? It turns
out (Aaronson says so anyway) that we can approximate any
transformation if we have just a few basic transformations.

Let's consider the Haddamard transformation:

    1/sqrt{2} 1/sqrt{2}
    1/sqrt{2} -1/sqrt{2}

This maps `|0>` to ``1/sqrt{2} (|0> + |1>)`. But interesting note: if
you do it again, you'll get `|1>`. This effectively derandomizes the
value of the qubit when observed. It does this via constructive and
destructive interference. That happens because of the negative sign,
btw. Aaronson says we don't need complex amplitudes for quantum
computing; that we only need negative probability amplitudes (I think
he says so?).

We have systems with multiple qubits. These systems are formed by the
**Kronecker product** of `C^2`. This is a little different than the
normal "tensor product." I believe the tensor product `(1, 0) \tprod
(0, 1)` merely **glues together** vectors. This would mean that a
three qubit system would be described by six dimensions. That is **not
correct**.

A three qubit system is described by an 8 dimensional space. There is
one amplitude for each bit string `000, 001, 010, 011, 100, 101,
111`. These are the basis states. We often write `|101>`. We might
also write `|5>_3` to mean `|101>`. The subscript 3 here is indicating
that this is in the 3-qubit space.

Note: while a `k` classical bit system's state is in a `k` dimensional
space, a `k` quantum bit system's state is in a `2**k` dimensional
space.

## Superposition, Separability, Entanglement

The basis states are like `|0>, |1>`. Any non-basis state is called a
**superposition** of basis states. We've seen that superposition means
that a variable can kinda be both zero and one, and that when
calculating the result of a unitary transformation we can see
constructive and destructive interference.

Classic superposition is `1/\sqrt{2} |0> + 1/\sqrt{2} |1>`.

We can consider basis states of multi-qubit systems. Again, a basis
state could be like `|101>`. A multi-qubit system is only in a basis
state if every qubit considered independently is in a basis state.

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

## Quantum operations

Quantum gates apply unitary matrices. One weird phenomenon is
important to note.

Consider if we try to transform a 2-qubit system by leaving one qubit
alone, and applying `U` to the second qubit? We can see this operation
as `I \tensor U` (tensor product of matrices). Could we not more
simply apply `I` and `U` separately to each qubit?

The very surprising answer is **absolutely not**! It **is safe** to
apply each operation separately **if** the two qubits are not
entangled. But if they are entangled, this is a no-no!!

Consider, please, this interesting scenario. Consider the single-qubit
system in the state `1/\sqrt{2} (|0> + |1>)`. I have already said that
applying the Haddamard again will give you `|1>`.

On the other hand, consider the two qubit system `1/sqrt{2} (|00> +
\|11>)`. This is entangled: it is the **EPR pair**.Say that I want to
apply the Haddamard to the second qubit (and do nothing to the first
qubit).

Let's first do this the right way: `I \tensor H`. Then this matrix is:

    1/\sqrt{2} *
    [ 1  1  0  0 ]
    [ 1 -1  0  0 ]
    [ 0  0  1  1 ]
    [ 0  0  1 -1 ]

You will get:

    1/2 *
    [  1 ]
    [  1 ]
    [  1 ]
    [ -1 ]

See what **didn't happen**?? The last `-1/2` for `|11>` does not
cancel out the `+1/2` for `|01>`!! That's because you can't interfere
across different values of qubit1 when you apply a transformation just
to qubit2.

**TODO**: CONTINUE!! ONWARD!!

## Sources

* Scott Aaronson
* Quantum Computing Without the Physics (Italian TJ Watson guy).
