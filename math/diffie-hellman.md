I think it sounds pretty easy. You choose a prime `p`. You choose a
primitive root `g` (they all are primitive roots).

Alice sends Bob `g**a mod p`. Bob sends Alice `g**b mod p`. Alice can
take `g**b` and raise it to the `a`th power. And Bob can take `g**a`
and raise it to the `b`th power.

But if the discrete logarithm problem is hard, then finding `a, b`
from `g**a` or `g**b` should be hard.

If you can share the secret `g**ab`, then you can use it as an
encryption key. DH is a good way to do perfect forward secrecy, since
DH key generation is fast. RSA generation is presumably much slower
(and each party needs to do it).

You can use Diffie-Hellman to do public key encryption. Basically,
Alice publishes `g**a, p` as her public key. Then, when Bob wants to
send a message, they pick a `g**b`, and send that in addition to their
message encrypted with `g**(ab)`.

But it's very interesting. This is still not *asymmetric*
encryption. Which means you can't do key signing with it.
