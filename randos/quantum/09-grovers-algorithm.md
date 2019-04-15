Grover's algorithm is an algorithm for black box search. It gives a
quadratic speedup over the typical methods.

To solve deterministically, the worst case time complexity is `O(2^n)`.
To solve probabilistically, you have expected run time of `O(2^{n-1})`.
It's because you have no intelligent strategy for testing.

So here is what we do. We assume the search function `f` applies a Pauli
X if the search criteria is satisfied. We put all inputs into a
superposition using the Hadamard tensored. For the "target" qubit, we
feed in `1/(\sqrt{2}^n) (|0> - |1>)`). This is `H |1>`.

We then apply `f`. In theory this flips the zero and one states of just
the sought for input. But in fact it maps

    1/(\sqrt{2}^n) (|0> - |1>)
    ->
    1/(\sqrt{2}^n) (-|0> + |1>)
    ->
    -1/(\sqrt{2}^n) (|0> - |1>)

Basically, we can flip the amplitude for the sought `x_0` to be
*negative*. Or another way: we're storing a new amplitude of
`2/(\sqrt{2}^n)` on the "opposite" side.

We now apply the tensored Hadamard again to the inputs. This has two
effects:

(1) It returns all the original amplitude back to `|0>`: 1.0.

(2) It splits apart the `2/(\sqrt{2}^n)` on the "opposite" side. Each
state gets an opposite amplitude of `2/(\sqrt{2}^{2n})`.

At this point, I want to bring everyone on the same side again. You can
construct a gate that flips the phase of just `|0>_n`. It's not hard.
Just takes some Pauli gates.

Now, if you apply the Hadamard once again, you break apart that
amplitude of 1.0 on `|0>`, and you reassemble the "stolen" amplitude of
`2/(\sqrt{2}^{2n})` on `x_0`.

*But* you do lose one thing. When doing the flip on `|0>_n`, it's as if
you introduce a new amplitude of `4/(\sqrt{2}^{2n})` that works against
*everyone equally*. Therefore, everyone's amplitude is decreased by
`4/(\sqrt{2}^{3n})`.

As you increase the amplitude on the correct `x_0`, eventually the
amount that is broken up from the amplitude on the "opposite" side
becomes greater than the fraction of the remaining amplitude that will
be stolen in the next round. Which is to say: you can only boost the
amplitude so high. But that only happens when a sliver `1/sqrt{2}^n` of
`x_0`'s amplitude is now worth more than `1/sqrt{2}^n` the *total*
amplitude on everyone else.
