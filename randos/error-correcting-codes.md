## Error Detection: Parity

First off, you have can just have parity bits per block. These do not
allow you to correct errors. They will, however, allow you to detect
errors. If this is a transmission, then you can re-request that
block. With a single detection bit, you can detect a single error 100%
of the time. However, you can only detect a two-bit error 50% of the
time. Basically, you can detect ~50% of errors.

If you have two bits of parity, you give the last two bits of the
count of ones in the message. This should give you ~75% error
detection.

Another way is the longitudinal redundancy check; this basically breaks
the message into bytes, and then XORs the bytes. This is "longitudinal"
in that it checks every 8th byte in the stream. I think, under some
assumptions, it should be as good as above. Note that two bits will
detect a single transposition.

A suffix like this appended to a message is called a *checksum*. It is
called *redundant* because derived data is being added to the message
stream.

The IBM System/360 had a *9 track* recording medium. It had 8 data
tracks and a 9th track for the XOR of the other 8 bits. This is a
"transverse" in the sense that it was computing parity *across* several
streams, one bit at each time point. Functionally this is the same idea
as longitudinal parity.

## Error Detection: CRC

Cyclic Redundancy Checks work like this. Basically, your bitstring
represents a polynomial with one-zero coefficients. You divide it by
another polynomial, of length 33, with some specified coefficients. This
divisor polynomial is called the *generator polynomial*. At the end, you
get a (polynomial) remainder of length 32. This is your checksum.

Note: you are working with polynomials over the finite field of two
elements: Z/2Z.

Example: parity is a CRC! It uses the generator polynomial x+1 (encoded
11). So:

* 0x + 0 => 0 (x + 1) + 0
* 0x + 1 => 0 (x + 1) + 1
* 1x + 0 => 1 (x + 1) + 1
* 1x + 1 => 1 (x + 1) + 0

**Calculation**

Why is this called *cyclic*? The reason is because poly long division is
cyclic. Imagine:

11010011101100 <- msg
1011           <- divisor
--------------
1 <- result
01100011101100 <- remainder
 1011
--------------
 1
00111011101100 <- remainder
  et cetera

Basically, you keep shifting through the stream, XORing whenever the
first bit is a one. You pad the stream by three bits to just keep the
final remainder. (Hmm, can I explain that?)

Choosing the generator polynomial involves balancing a number of
concerns that I mostly don't understand/care about. But once you choose
the number of bits to use, you'll still have a lot of generator choices.
There are a number of choices for CRC-16, CRC-32, et cetera. The 16 only
specifies that you'll use a degree 17 polynomial (leaving 16 bits for
remainder).

## Mathematics of CRC

The math of CRC is basically just the ring of polynomials over Z/2Z,
except it's the *quotient ring* where you mod out by the generator
`p(x)`. I'll assume that `p` has degree `n`.

So say you get the message, and you want to compare to the CRC. You'll
detect an error exactly when the XOR of the message received and the
true message is a multiple of `p(x)`. We write the error polynomial as
`e(x)`.

Say there is a one-bit error. Then `e(x) = x^k` (for some `k`). This is
only divisible by `x^i` for `i < k`. Note that if `p(x)` has two bits
popped, think about multiplying it by any `q(x)`. The two highest degree
terms will always multiply together and survive (no one else can touch
them) and the two lowest terms will also. So if `p(x)` has two bits
popped, it cannot divide `x^k`.

Next, we consider a *two bit* error. This has `e(x) = x^k_1 + x^k_2`.
Which has the form `x^k_2 (x^{k_1 - k_2} + 1)`. We know that `p(x)`
won't divide `x^k_2`, so can it divide `x^{k_1 - k_2} + 1`?

This effectively asks: is `x^{k_1 - k_2}` equal to `1` mod `p(x)`. Put
another way: what is the order of `x` in the quotient ring? If `x`
generates the entire group, then it hits all `2^n - 1` elements. (Note
I'm assuming that we are working in a quotient *field*. This happens
exactly if `p` is irreducible!).

If `x` generates the entire group, then `x^{2^n - 1}` is the first power
of `x` that is equal to `1` mod `p`. So the CRC will detect any
two bit error if they are separated by fewer than `2^n - 1` bits!

Such a `p` that admits such an `x` is called a *primitive polynomial*.

## Protocols

* Automatic Request Repeat
    * Just detect errors and ask for retransmit.
* Forward Error Correction
    * Fix errors that you detect with what you deem most likely.
* Hybrid
    * Fix minor errors you detect, ask for retransmit of major errors
      you detect.

## Error Correcting Codes

You want to be able to correct codes without re-requesting. This is
important for disks (since the writer will forget the data), phones
(where the latency to re-request is too high).

A simple way is to send three versions of each bit. The user just
takes the majority vote. But this is pretty ineffecient for fixing a 1
bit error.

Hamming gives us a way to correct *any* 1 bit error. You transmit 4
bits of data, and 3 parity bits. Here's how:

```
P1 = D1 XOR D2 XOR D4
P2 = D1 XOR D3 XOR D4
P3 = D2 XOR D3 XOR D4
```

Let's say we know at most one bit is wrong. Let's show how to correct
this. Let's say we receive:

* 0000 000
    * We know immediately that no data bits could be wrong, since then
      one of the parity bits would have to be wrong, too.
* 0000 001
    * Since the parity bit is checked, then either it is wrong, or
      some data bit (D2, D3, or D4) is wrong.
    * But that would mean *other* parity bits should have been
      checked, so that is not possible.
    * So it's the *parity* bit that's wrong!
    * This logic holds for any of the other parity bits.
* 0001 111
    * We know immediately that none of the data bits can be wrong,
      since otherwise one of the parity bits is wrong.
* 0001 110
    * If the parity is right, then D2 or D3 should be checked, but
      this is not possible, since then other parity bits are wrong,
      too.
* So one bit errors in the parity don't cause mistakes.
* 0001 000
    * We can see here that the data bit must be wrong, else *two*
      parity bits are wrong.

So what we're seeing here is that all 1 bit errors can be
corrected. Another way of looking at it: the Hamming distance between
any valid Hamming codewords is 3. That's why you can detect two
errors, and correct one.

Hamming is widely used for ECC memory; Hamming codes are most useful
when the error rate is low. With high error rates, you're not going to
do that well.

Generalizations of Hamming codes can acheive higher data rates. You
can also extend Hamming to detect two bit errors, too, by including a
bit that xors all the Hamming bits.

## Reed-Solomon

Reed-Solomon codes are widely used, they are error-correcting, and can
also detect missing data. Codes like this are called *erasure codes*.

Reed-Solomon, given `t` check symbols, can:

* Detect `t` erroneous symbols.
* Correct `FLOOR(t/2)` erroneous symbols.
* It can correct up to `t` erasures.

With larger symbols, you can handle burst errors well, since this
corrupts a single (or a few) symbols. This makes it useful for CDs,
DVDs, where burst errors are common (misaligned laser).

When Reed-Solomon codes were created, an *efficient* decoding scheme
did not exist. That came almost a decade later, which allowed for
their practical use.

The math looks pretty hard on this, and I don't really care that
much. Let's just say **Mission Accomplished**.

# TODO

* Rolling Hash
* md5sum
