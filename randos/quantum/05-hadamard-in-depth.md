## Hadamard Operation in Depth

(This reviews in a little more depth what I've already said about the
Hadamard elsewhere).

As we've seen, Hadamard on one qubit involves: reflecting the y
coordinate, then rotating 45deg CCW.

Hadamard moves a basis state to a uniform superposition. But unlike a
45deg rotation, it is also its own inverse. So it also maps a uniform
superposition back to a basis state.

I see this as: it can simultaneously be "reassembling" a split-up
quantum state while "exploding" a basis state. We'll see this help us in
Grover's algorithm.

Let's recall think of what `H^{\otimes{n}}` does to `|0>_n` (`n` qubits
with value `|0>`). It maps this to every state `|x>`, where the
amplitude is equal to `\frac{1}{\sqrt{2^n}}`.

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
