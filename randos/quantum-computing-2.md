## Basics

Sometimes you'll see `<0|` which means the conjugate transpose of
`|0>`. So `<0||0> = 1`. We simplify and just write `<0|0>` (no double
bar) to mean the inner product of `|0>` with itself.

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

* Continue onward to Grover's Algorithm.

## Sources

* Scott Aaronson
* Quantum Computing Without the Physics (Italian TJ Watson guy).
* https://quantumcomputing.stackexchange.com/questions/4430/quantum-teleportation-second-classical-bit-for-removing-entanglement

# Chapter 1

You can prepare a bunch of qubits, putting each in superposition. You
can then use this as the input to a quantum circuit, which effectively
will calculate the result for *all* the possible bit
settings. However, at the end, if you'd measure, you'd only really get
the value for *one* setting. So the question is if you can calculate
some *global* property of the function. In a sense: you put in `n`
qubits, which, in superposition and tensored, represent `2**n` states,
but at the end you only get `n` bits of information after
measuring. That's *super handwavy* but it gives the general idea.

Talks about use cases: Shor's Algorithm for factoring (exponentional
to quadratic), Grover's algorithm for search (linear in size of search
space to sqrt), and quantum simulation. Other use cases aren't yet
known.

## Random Thought

We think it's weird that "observation" determines reality. But
observation is *not* subjective; it involves a process of
*measurement*, which necessarily involves *constraint*.
