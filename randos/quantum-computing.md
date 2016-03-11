# Chapter 1

Follows Aaronson, mostly. Have quantum states, they have certain
probability when measured. Can have independent qubits, which are
tensored.

Controlled not gate (2 qubit gate; it's unitary) and (1 qubit) unitary
transformations are all we need for quantum computation. Hadamard and
controlled not can yield Bell (aka EPR) pairs, which are the way we
"mixup" two qubits.

For instance, we can do quantum teleportation. Teleportation is
interesting: you can communicate in two bits the state of your qubit
to another person (provided you already shared an EPR pair). The
person on the other end gets your qubit state, but your qubit is
destroyed (you measure it in the process). Note that, as always,
neither you nor your correspondent truly know the hidden qubit state.

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
