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

Let's consider the Hadamard transformation:

    1/sqrt{2} 1/sqrt{2}
    1/sqrt{2} -1/sqrt{2}

This maps `|0>` to ``1/sqrt{2} (|0> + |1>)`. But interesting note: if
you do it again, you'll get `|0>` again. This effectively derandomizes
the value of the qubit when observed. It does this via constructive
and destructive interference. That happens because of the negative
sign, btw. Aaronson says we don't need complex amplitudes for quantum
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
applying the Hadamard again will give you `|1>`.

On the other hand, consider the two qubit system `1/sqrt{2} (|00> +
\|11>)`. This is entangled: it is the **EPR pair**.Say that I want to
apply the Hadamard to the second qubit (and do nothing to the first
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

## No Cloning

Basically, there is no unitary transformation which maps `|psi>
\otimes |0>` to `|psi> \otimes |psi>`. Which means you can't clone
a quantum state (unless you already know what the state is).

This means that quantum state is, in a sense, truly "private." If we
could clone psi, we could clone it a lot, and keep trying to measure
in the computational basis. That would tell us the squares of the
probability amplitude.

We could then keep doing that in the Hadamard basis, to gain more
information. If we keep at it, we can figure out what psi is through
statistical testing.

That's what no cloning says we *can't* do. We can't clone, so we can't
run these tests.

Aaronson suggests that this is like trying to figure out the fairness
of a coin based on one coin flip.

## Quantum Key Distribution

I want to agree on secret key bits with you. We assume we have a
quantum channel, and an authenticated classical channel. An
authenticated channel means that Eve can eavesdrop but cannot pretend
to be either of us. What kind of scenario could be like that: if I can
authenticate can't I do crypto? Maybe the authentication comes from
some factor that can't be mimicked, like your voice.

Anyway, I send you some qubits, prepared either in the `|0>, |1>`
computational basis or the Hadamard basis: `\frac{1}{\sqrt{2}} (|0> +
|1>)`.

I send you 32 qubits, each one prepared in a 50/50 choice of basis. 16
will constitute our key, and 16 will be used for checking for
eavesdroppers.

How do we know eavesdroppers haven't observed the 16 qubits you're
going to use as a key? Well, we're going to test them with the other
16 qubits. You randomly choose a basis to measure each qubit in. You
will send the results back to me. You will say what basis you measured
in.

For those 16 qubits, you probably measured half of them in the wrong
basis. But for the other half you do choose correctly. I know what the
right answers are in that basis.

Now, if Eve had eavesdropped one of those eight qubits, then after
measuring, she needs to put something back in the channel. But here's
the problem: how is she going to know how to prepare the qubits again
for replacement? Measuring the eavesdropped qubits doesn't give her
any information about the basis they were prepared in.

So if Eve puts a qubit back in, she has a 50% that she'll put it in
the wrong basis. Then there's a 50% chance it's selected for
parity. Then there's a 50% chance that Bob measures it in the right
basis. If all those things happen (1/8), then Alice's eavesdropping is
detected.

Of course, you can amplify this by using more parity qubits. And you
throw out everything and start again whenever you detect
eavesdropping.

## Simon's Algorithm

We have a function `f` defined on `n` bits where for some `a` `f(x) =
f(y)` iff `y = x + a (mod 2**n)`. We want to find `a`.

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

    H^{\otimes n} |j> = \sum_{i = 0}^{2^n - 1} (-1)^{i \cdot_n j} |i>

(`\cdot_n` means bitwise dot product mod 2. i.e., do `i, j` share an
even number of bit values in common?).

## Hadamard Operation in Depth

What the fuck does that mean? Let's consider Hadamard on one
qubit. Remember that Hadamard is equal to: reflect y coordinate, then
rotate 45deg CCW.

So, applying Hadamard to the computational basis gives you a new
basis: `\frac{1}{\sqrt{2}} (|0> + |1>)` and `\frac{1}{\sqrt{2}} (|0> -
|1>)`.

So lets apply it again to these basis states. Hadamard is its own
inverse. Lets consider what `H` does to `H|0>`. It takes the
`\frac{1}{\sqrt{2}} |0>` part of `H|0>` and gives this some `|0>` and
some `|1>` component. At the same time, `H` transforms the
`\frac{1}{\sqrt{2}} |1>` part by giving it some `|0>` component, and
some *negative* `|1>` component. Thus, the `|0>` parts that `H`
creates from `H|0>` combine constructively in the `|0>` basis
dimension, but combine destructively in the `|1>` basis
dimension. That's how we get back to `HH|0> = |0>`.

Let's think of what `H^{\otimes{n}}` does to `|0>_n` (`n` qubits with
value `|0>`). It maps this to every state `|x>`, where the amplitude
is equal to `\frac{1}{\sqrt{2^n}}`.

Now lets apply the Hadamard to try to go back. Then every `|x>` value
contributes some amplitude to every `|y>` value. If we were just
working with one qubit, then we know that every value (both `|0>` and
`|1>`) gives positive amplitude to `|0>`, while only `|1>` gets mapped
to contribute negative value to `|1>`.

We can now consider applying the Hadamard operation iteratively to
each qubit of the `n` qubits. If the first qubit is `|1>`, this means
it contributes an amplitude of `\frac{1}{\sqrt{2}}` to `|0>` and of
`-\frac{1}{\sqrt{2}}` to `|1>`.

If the next qubit is also `|1>`, then we will multiply the first
amplitudes by `\frac{1}{\sqrt{2}}` and `-\frac{1}{\sqrt{2}}`
again. And this is how we get the equation that we get the equation
that `|x>` maps to:

    \sum (-1)^{count of 1s in common to both x and y} |y>

## Back to Simon's Algorithm

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

The idea is to iteratively sample `j`s, and build up a system of
linear equations. We do like this:

    J a = 0 (mod 2)

Where our `j`s are rows in `J`, the `a` is a column vector we're
searching for, and `0` is a column of zeros, because rows of `J`
always have an even number of ones with `a`.

Once we have sampled `n` linearly independent `j` values (considered
mod 2), then the system will have at most one non-zero solution for
`a`.

**TODO**: I'm a little fuzzy on how many linearly independent vectors
`j` are needed to determine a unique solution `a` given that the
matrix does not have a unique inverse.

Anyway, it can be shown that if you sample vectors randomly, then you
very quickly get a linearly independent spanning set. The probability
of `n` linearly independent vectors amongst `n+k` samples is bounded
below by `1 - 1/(2**k)` (presumably fairly easy result of Mermin
2007).

So with `O(n)` executions, you are very likely to choose a set of
vectors `j` that constrain `a` to exactly one non-zero value. Doubling
the number of executions much more than doubles the odds (by above
result).

It feels like this algorithm is in the quantum analogue of ZPP AKA Las
Vegas algorithms.

## Grover's Algorithm

Black box search with a function that outputs 1 for exactly one input.
(Works fine if there are more than one positive input; you'll see.).

Here's the idea. You're going to put things in a superposition, then
flip the amplitude of the solution state to negative. At this point, you
can subtract the state from 2x the average amplitude. This will reduce
the amplitude of all the wrong states, and increase the amplitudes of
the good states (negative minus negative, right?).

Notice how this works until the average amplitude goes negative.
Let's check it out:

  (# of bad answers) * amplitude_of_each_bad_answer
  = (# of good answers) * amplitude_of_each_good_answer

Since we are going to have to square amplitudes, we can get this highest
if there is exactly one good answer.

* TODO: Talk about number of iterations.
* TODO: I think you can get max constant prob of a good answer.
* TODO: But then you can just repeat a few times to get any prob
  desired. As long as you can get over 51% (not 50% + eps!) in poly
  time, you can get over 99% in polytime.
* TODO: Talk about how to build this operation from gates.

## TODOs

* Quantum teleportation
* Continue onward to Grover's Algorithm.

## Sources

* Scott Aaronson
* Quantum Computing Without the Physics (Italian TJ Watson guy).
* https://quantumcomputing.stackexchange.com/questions/4430/quantum-teleportation-second-classical-bit-for-removing-entanglement
