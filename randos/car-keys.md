Okay.

Traditionally, the same car key opens the doors and turns the ignition.

Car keys used to be shaped like door keys.

Nowadays, car keys are often "internal cut", also called "sidewinder" or
"laser cut." That just means that the blade of the key is engraved with
a wavy pattern. Maybe those are harder to clone; they need special
machinery anyway. They have the convenience of being insertable in
either direction (cut on both sides).

Most cars today have transponders embedded in the plastic keybase. When
a key is inserted in the car, the car sends a radio signal to the
keybase. This energizes a circuit in the keybase, which then sends back
a response.

One way to design the system is to have the car issue a nonce challenge.
The fob then signs the nonce with the private key embedded in the key.
The immobilizer verifies the signature against a list of public keys
stored in the immobilizer.

This would have the advantage that aftermarket keys could be laser cut
with the correct pattern, AND you could allow aftermarket crypto keys to
be generated.

Because the private key is embedded in the key, it would hopefully be
hard to read out of the key.

This system works if it is hard to add an unauthorized key to the
immobilizer. One way to do this is to make it hard to add keys to the
immobilizer.

But there has to be equipment at dealers that adds keys to an
immobilizer. That could be stolen. Then the thief can use that forever
to program new blank aftermarket keys to steal cars.

Here are some ideas:

1. Have the immobilizer reject new key additions unless there is a
   signed statement from an authority (the manufacturer) saying this key
   is approved for this VIN.
2. Have the key circuitry be VIN specific. Generic keys cannot be
   manufactured. The key won't respond except to the specific VIN. Keys
   can only be produced by the manufacturer.

Method #1 requires that new keys require registration from the
manufacturer. But it still allows for a generic market for keys. The
customer could buy an aftermarket key and ask the manufacturer to sign
it.

The key authorization (signature by manufacturer) could be sent to the
dealer, which would only upload it to the immobilizer on verification of
the individual.

However, the manufacturer might not like the aftermarket, and might
refuse to sign aftermarket keys. This might be justified if they think
that the aftermarket might be compromised by thieves and that they could
produce weak keys or that they retain/sell the private keys. Or the
manufacturer just wants to control the market for keys.

Is #1 necessary? Must the immobilizer be programmed? Why can't the key
both (a) sign the challenge, and (b) show the manufacturer's signed
certificate that this public key is approved for this VIN?

That eliminates the immobilizer upload process. But it requires that the
key is special-made for the vehicle (basically #2 proposal). That makes
it slow to acquire new keys.

Ideally, keys are in stock and the dealer needs only (1) verify the
customer, (2) cut the physical key, (3) ask the manufacturer to issue a
signature for the in-stock fob. This is possible if the key is not VIN
specific.

Source: https://www.cs.tufts.edu/comp/116/archive/fall2015/arichardson.pdf
