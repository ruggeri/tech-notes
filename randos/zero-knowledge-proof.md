## Zero Knowledge Proof

Classic example. Prove you have a 3-coloring of a map. This is an NP
complete problem.

You color the map. You hash each color (with a nonce). This hides the
values. You send the hashed map to the validator.

You let the validator ask to reveal two adjacent vertices. You
give the hash pre-image, and they can verify this.

You can repeat as many times as you want; but you need to permute the
colors used each time. This gives no info about the coloring.

Note: this is an _interactive_ protocol for proving.

Note that because any NP problem (that is, polytime verification) can be
encoded as 3-SAT, you can then do a zero knowledge proof for any such
problem. That is: you can prove that you have a proof, but without
providing any information about how to make a proof.

## Discrete Log Example

- Peggy knows `x`, the discrete log of `y = g^x mod p` base `g` in the
  multiplicative subgroup of `Z mod p`.
- Maybe this is a proof of identity: Peggy chose `x` randomly and has
  revealed it to no-one. She has distributed `g^x`.
- Peggy wants to prove she knows `x`, but she doesn't want to reveal
  anything about it.
- Thus, Peggy chooses a random `r`, and produces `C = g^r` for Victor.
- Victor can either:
  - Ask for `r`, to verify that Peggy knows the discrete log of `C`.
  - Ask for `z = x + r`. Victor can then verify that `Cy = g^z`.
  - Note that we can take `z = x + r mod p-1` because Fermat's Little
    theorem.
- It's very important that Peggy cannot predict Victor's question.
  - If she knows he'll ask for `r`, then she can choose any `r` to
    generate `C = g^r`.
  - If she knows he'll ask for `z`, she can pick any `z`, calculate
    `g^z`, and then multiply by `y\inv` to get the appropriate `C`. Note
    that `y\inv mod p` can be easily calculated using Euclidean
    algorithm.

## Definition of Zero Knowledge.

- We know that a ZKP is a protocol for proving something. We know that
  the probability of the proof succeeding should be at least 2/3, while
  the probability of the proof failing should be at least 1/3. We can
  always amplify to an arbitrary accuracy.
- The protocol is _interactive_. Both actors can use randomness.
- It should be **zero-knowledge**. But what exactly does that mean?

Imagine that Victor shows Peggy's proof to a third party Eve. Does
Victor's transcript of his interaction with Peggy convince Eve? Not if
Eve doesn't trust that Victor truly used random numbers. Otherwise,
Peggy and Victor could simply be colluding.

Even more: maybe Victor didn't even talk to Peggy at all. Why bother
colluding? Since Victor gets to pick the challenges, he can fake the
correct answers. So a transcript shown to Eve would be utterly
unconvincing.

And this gives us the hint on how to define **zero-knowledge**. If the
transcript of a true run of the protocol with Peggy is something Victor
was equally likely to have faked by himself, then he didn't really learn
anything from interacting with Peggy.

Example: Peggy is a telepath. She asks Victor to pick a number, and then
she guesses it. This is very convincing for Victor. But it doesn't teach
him anything about how Peggy does it. The transcript ("Think of a
number." "Ready." "It is seven." "Correct!") is exactly what you would
dream up despite knowing nothing about how the telepathy works.

## Perfect (Statistical) and Computational Zero Knowledge

- Ideally, the statistical distribution on true transcripts with Peggy
  is equal to a generator of transcripts that Victor could run without
  interacting with Peggy. In this case, the protocol is _statistical
  zero knowledge_, also called _perfect_.
- Less strong is _computational zero knowledge_. This says that no
  program can efficiently distinguish true transcripts from fake
  transcripts.

## Zero Knowledge Password Proof

A bad way to do login is to store a password on the server, and upload
that password to login. The server then verifies the password by
checking if they are equal.

You can make this slightly better by using a PBKDF: basically a salted
hash of the password. This will mean that it's harder to do a dictionary
attack against the verifier stored on the server. This protects you from
the server being hacked.

However, anyone who can eavesdrop the traffic will be able to read your
password off the network. To defeat this, you could do Diffie-Hellman
with the remote server to establish a session key.

This would defeat an eavesdropper, but not someone who can MITM. So the
remote server had better have a public key which you can verify and
which it can sign its DH message with.

But now let us say that the server can be hacked. The hacker can steal
the verifier, but it is of limited use if it uses PBKDF. But the hacker
can now MITM you because it has the secret key.

If you are sending the server your password, then you are revealing this
to the hacker.

This is particularly problematic if you reuse passwords on different
websites. The ability to reuse passwords would be desirable.

## Comparison With Private Key

A common login method is you upload a public key on setup, and then you
must sign nonces sent by the server with your private key. But this
requires you to have a device like a Yubikey. It makes it harder to log
on from other machines, and having a device like a Yubikey could be one
more thing that could be stolen.

Jonathan suggests that you could upload your private key encrypted with
a password to the remote server. The remote server could present this to
you, which you could then use your password to decrypt.

## Sources

- Personal communication: https://mail.google.com/mail/u/0/#inbox/KtbxLzGLkSbsHbrxhrPhzDskndMKpDbQDV
- Slightly more useful source: https://pdfs.semanticscholar.org/2880/993c1abc110e832422753a5134f8ccf0633b.pdf
