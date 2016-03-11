Of course, there is the **one-time pad**, which, if used correctly,
cannot be broken, since every message decoding is equally likely given
the ciphertext.

However, OTP requires that the users share a key. Key distribution is
difficult, especially since parties who have never met before may wish
to communicate.

Note that OTP is still susceptible to some attacks. If an attacker
knows a portion of the plaintext, and can modify the encrypted
message, they can change this part of the message
surreptitiously. This is called malleability. For that reason, you
want to use a hash function at the start to ensure the message is not
tampered with. Note that message integrity (resistance to
malleability) is different from signature.

A solution is to use Diffie-Hellman so that two parties, over an
unsecured channel, can agree to a session key to encrypt their
communication. This means you solve key distribution. DH works like
this:

* The two parties agree on a shared number.
* They each mix the shared number with a one-time, secret number.
* They each send their mixture to the other. It should be hard for an
  adversary to unmix either.
* Each party mixes their own secret number into the other's
  mixture. The parties both get the same result.
* It needs to be hard for the adversary to get the final mixture from
  the two mixtures he observed.

DH presents a problem of authentication. Who are you communicating
with when you use DH: could it be MITM? For this reason, you want
proof of identity. This is typically done by signing a nonce with a
public key, using a cryptosystem like RSA. Without digital signatures,
you'd never know who you were talking to.

Digital signatures means signing a hash of a message with a private
key, which anyone with the public key could verify. It is easier to
publish everyone's public key in a trustworthy way (this is Public Key
Infrastructure; SSL is built on a PKI with certificate
authorities). To establish a connection, each step of DH should be
signed so that both parties know the other is participating in DH.

Note that signature becomes important when you don't have a
pre-arranged private key: a private key would have implicitly
authenticated the other party.

Typically, after key agreement, a symmetric cipher is then used for
efficiency.

## Forward Secrecy

The idea is that past secure communication should remain secure even
in the future. To acheive this, you want to use something like DH to
establish session keys that will later be discarded. I believe this is
what SSH does.

## Password Storage

Typically you want to store a hash of a password, so that the password
cannot be stolen and used elsewhere. And you want to use a salt for
the password before hashing; else someone can hash a popular password
and break everyone's rapidly. But it doesn't matter unless you use a
different salt for everyone. This forces passwords to be cracked one
at a time.

You can further frustrate and adversary by using a slow hashing
function.

## Theory

I think that assymetric crypto can't exist without trapdoor OWFs. OWFs
aren't known to exist; trapdoor OWFs aren't known to exist even given
OWF. OWFs would imply `P!=NP`, but it is not known whether `P!=NP`
implies OWFs.

OWFs would imply both cryptographically secure pseudorandom number
generation (in fact, each implies the other; makes sense) and digital
signatures (wait, don't you need trapdoors?).

Cryptographically secure hash functions are OWFs with additional
properties that it should be difficult to find a second pre-image
given a first pre-image, or even to produce any pair of pre-images at
all.

I think RSA has not been proven secure even when integer factoriation
is assumed to be hard.

It looks like maybe DH can be modified for digital signatures? Thta's
curious; why aren't trapdoors needed?

## Secret Sharing

Basically, an m-of-n key. This is important when you want it to be
hard to steal something, but you don't want to make it too easy to
lose. You have high confidentiality *and* reliability.

It would be insecure (would reveal information) if you split a key
into parts and gave everyone overlapping sets of parts. Then anyone
knows *part* of the key.

Better would be, for each m-of-n subset, give everyone a copy of the
key, encrypted with each participant's public key.

But that's not practical if you want, say, 50-of-100 sharing. That's
where Shamir Secret Sharing comes in.

There are lots of twists. First, even if the underlying secret can't
change, you can frustrate attacks to steal shares slowly by reissuing
new shares, having everyone delete their old shares. You might also
want to be careful: before revealing your secret, maybe you want to
make sure everyone really does possess a secret. Maybe they're trying
to con you and see your secret even though they don't possess
one. They could repeatedly do this to each party. That's a zero
knowledge proof scenario.

BTW: Shamir is basically that k points determine a k+1 dim curve. So
if you want m-of-n, you sample n points of a m+1 dim curve.

## TODO2

* Secure multiparty computation.
