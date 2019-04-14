**Single Qubit Gates**

In theory, any unitary transformation of `C^{2^k}` is a valid
transformation of a `k`-qubit quantum state.

The simplest gates are 1-qubit gates. Here are some classics:

    Identity
    1 0
    0 1

    Pauli X (effectively a not gate)
    0 1
    1 0

    Pauli Y (haven't actually seen this used yet)
    0 -i
    i  0

    Pauli Z (called phase flip gate)
    1  0
    0 -1

None of these will transform you from a basis state to a superposition.
The most common choice is the Hadamard transformation `H`:

    1/sqrt{2}  1/sqrt{2}
    1/sqrt{2} -1/sqrt{2}

**Simple Multi Qubit Gates**

Okay, but what about gates that operate on multiple qubits? Before we
address that, what is the transformation that applies the Pauli X on
qubit #2 while leaving qubit #1 untouched?

    X \otimes I
    0 0 1 0
    0 0 0 1
    1 0 0 0
    0 1 0 0

Right? So even if we apply a gate to a single qubit, it is as if
applying a (simple, Kronecker-decomposable) unitary transformation to
the entire system.

A common "product gate" is `H \otimes H`:

    1/2   1/2   1/2   1/2
    1/2  -1/2   1/2  -1/2
    1/2   1/2  -1/2  -1/2
    1/2  -1/2  -1/2   1/2

Note that the Hadamard product moves the `|0>_k` basis state to a
uniform superposition over all basis states.

Another note: the Hadamard moves any basis state `|i>_k` to a uniform
superposition over states `|j>_k`, but the *polarity* depends on the
bitwise dot product `i \cdot j (mod 2)`. Basically: only the
*maintenance* of a one bit causes a polarity shift.

**Product Transformations Apply Easily To Product States**

Consider the unentangled state `|0> \otimes |1>`. Then applying `I
\otimes H` is simple: you simply apply `I` to |0>` and `H` to `|1>`, and
take their normal tensor product. You get `|0> \otimes (1/sqrt{2}) (|0>
- |1>)`.

Note how different it is with `(1/\sqrt{2}) (|00> + |11>)`. Here we get
something very different. We get:

    1/2 |00> + 1/2 |01> + 1/2 |10> - 1/2 |11>

(I don't know if this is an interesting or important point. I think it
once confused me, but that's before I really understood quantum state, I
think...)

**Measurement Gates**

We can measure a single qubit. If we measure qubit `x`, then the
probability of observing a zero is:

  \sum_i |\alpha_i|^2 (where bit x in i is zero)

We then renormalize in the obvious way. There is no difference between
measuring 5 qubits and measuring each of five qubits one-by-one.

**CNOT (XOR) Gate**

The first gate to consider is the *controlled not* (aka CNOT, aka XOR)
gate. It is:

    1 0 0 0
    0 1 0 0
    0 0 0 1
    0 0 1 0

Basically: the first qubit says whether to flip the second qubit. It's
like XOR.

Look closely: note how CNOT cannot be decomposed into a Kronecker
product of two independent operations on two different qubits? That's
what makes it an entangling transformation.

We can make a SWAP gate in the typical way:

* XOR Q1 into Q2 to get (Q1 xor Q2).
* XOR (Q1 xor Q2) into Q1 to get Q2.
* XOR Q2 into (Q1 xor Q2) to get Q1.

**CCNOT (AND, Toffoli) Gate**

The second kind of entangling gate we use is the CCNOT gate. It is like
an AND gate. It flips the third bit only if the first two are ones.

**Universality**

With the Pauli X (XOR) and the CCNOT (AND) gates, we can build any
boolean circuit. In fact, using just a few single-qubit gates we can
approximate any unitary transformation. I don't know the proof of this
theorem (Solovay-Kitaev), but I believe it.
