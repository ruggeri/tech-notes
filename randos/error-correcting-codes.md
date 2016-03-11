## Error Detection: Parity and Checksum

First off, you have can just have parity bits per block. These do not
allow you to correct errors. They will, however, allow you to detect
errors. If this is a transmission, then you can re-request that
block. With a single detection bit, you can detect a single error 100%
of the time. However, you can only detect a two-bit error 50% of the
time. Basically, you can detect ~50% of errors.

If you have two bits of parity, you give the last two bits of the
count of ones in the message. This should give you ~75% error
detection.

Another way is the longitudinal parity check; this basically breaks
the message into bytes, and then XORs the bytes. I think, under some
assumptions, it should be as good as above. Note that two bits will
detect a single transposition.

A generalization of parity is the *checksum*. This is any modular
arithmetic of the "words" in a message. Parity is the simplest
version.

However, note that these codes fail to detect reordering of the
"words". This is addressed by using the position of a word, in
addition to its contents. Addler-32 and CRC32 take into account the
position.

I think these schemes can't improve on detection of random errors, but
can detect many common kinds of errors. Basically, the data would have
to be scrambled *just right*.

CRC32 works like this. Basically, your bitstring represents a
polynomial with one-zero coefficients. You divide it by another
polynomial, of length 32, with random coefficients. At the end, you
get a remainder. This is your checksum.

This can be done fast in hardware (it's just bitshifting and XOR,
actually). And it is very positional, so that means a lot of common
error types are ruled out.

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
