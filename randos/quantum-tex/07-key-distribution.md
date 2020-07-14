## Quantum Key Distribution

Alice wants to agree on secret key bits with Bob; simply some ones and
some zeroes. They have an authenticated classical channel, but no
encryption on it. Eve cannot pretend to be either Alice or Bob.

Alice and Bob don't want to use an public key or asymmetric
cryptography. We only want to do private key crypto.

Alice and Bob have an unsecured quantum channel. People can eavesdrop on
it. People can tamper with it.

Alice generates 32 random bits. Alice then encodes these as qubits. For
each bit, Alice choose either the standard computational basis (`|0>,
|1>`) or the Hadamard basis: `\frac{1}{\sqrt{2}} (|0> + |1>)`,
`\frac{1}{\sqrt{2}} (|0> - |1>)`. Alice encode the bit appropriately.

Alice sends the 32 qubits. Bob confirms receipt. For each qubit, Bob
chooses randomly which basis to perform the measurement in.

Bob reveals to Alice his choice of bases, and Alice reveals hers. Where
they disagree (16 of the qubits probably), they throw those away.

Of the remaining 16, Alice chooses 8 for a validation check. She asks
Bob: what values did you get? He reveals the values. If they are all the
right answer, then Alice feels that the qubits were not tampered with.
The final 8 values can be used for the key.

## Secure Against Cloning

We know that Eve cannot clone the qubits that pass through the channel,
wait until Alice reveals her basis, and then Eve measures the qubits.
This is known by the no-cloning theorem.

## Secure Against Eve's Measurement And Replacement

Eve could try to measure the qubits passing through, and replace them in
the channel on the way to Bob. But the point is: Eve doesn't know the
correct bases to measure in. So she'll choose wrong sometimes, and then
Bob, when measuring, now only has a 50/50 chance of getting the answer
Alice expects.

## Secure Against Entanglement

Last, imagine that Eve entangles any qubits with a qubit sent by Alice.
For instance, perhaps she does a controlled NOT so that the state is:

    \alpha |0>_A |0>_E + \beta |1>_A |1>_E

Now, if Bob measures in the computational basis (that is, he doesn't
rotate before measuring), then because this pair is perfectly entangled,
Eve will learn Bob's measurement.

BUT, if Bob *does* rotate before Alice measures, then he won't get the
proper cancelation, because of the unmeasured entangled qubit that Eve
created.

As a specific example: Alice sends `(1/sqrt(2)) (|0> + |1>)`. Eve
entangles so the state becomes `(1/sqrt(2)) (|00> + |11>)`.

Bob goes to measure in the Hadamard basis (the correct choice). So he
applies `H`, and the state evolves to:

    |00> + |10> + |01> - |11>

But now Bob will have a 50% chance of getting the incorrect result.
Alice and Bob will detect Eve's tampering.
