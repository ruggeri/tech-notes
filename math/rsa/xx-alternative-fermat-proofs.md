I think the proof of Lagrange's theorem is plenty intuitive. But here
are some attempts at even "more" intuitive proofs of Fermat's Little
Theorem.

## Binomial Theorem Proof

I read this on Wolfram. It is inductive. Note the statement is true
for `a = 1` of course.

Use the Binomial Theorem:

    (a + 1)^p = (p choose p) a^p + (p choose p-1) a^{p-1} + ...
                (p choose 1) a + (p choose 0) 1

Note all the coefficients have the form:

     p! / (k! (p-k)!)

For any `k` st `0<k<p`, then necessarily this is divisible by `p`.

So we can say:

   (a + 1)^p = a^p + 1 (mod p)

But here we use the inductive hypothesis, that `a^p = a (mod p)`.

So we have

   (a + 1)^p = a + 1 (mod p)

Done!

## Permutations proof

We know that 1a, 2a, 3a, ..., is just a permutation of 1, 2, 3,
... Therefore we know that:

    (p-1)! = 1a * 2a * 3a ... * (p-1)a

Where the right side is permuted somehow. But this is:

    (p-1)! = (p-1)! a**(p-1)

Cancelation says:

    a**(p-1) = 1.

Donezo.

## Combinatorics

Consider necklaces of length `p`, where there are `a` choices of beads
at each spot. There are `a**p` necklaces. Of these, `a` of them are
constant color. Every other necklace of has `p` equivalent permutations.

That means that (a**p - a) is divisible by p. That means:

    a**p = a (mod p)
