## Ch3: Godel, Turing and Friends

**Godel's Completeness Theorem**

This says that everything which is true in every model of the theory
is provable. There are two ideas here: _logical validity_ of a
statement means that in every model of the theory the statement is
assigned true; a theory is called _complete_ if every logically valid
statement is provable in the system. Basically, Godel says that every
_sound_ theory (one where only logically valid formulas are provable)
must be complete.

How is this proven? Basically, you need to produce a model where
statements are assigned true if they are provable (this is required by
soundness), but where you are free to set any other statement true or
false as you desire. That means that all statements _not provable_ are
not logically sound.

This is shown by adapting a model that based on the language of the
theory. The quote "extracting semantics from syntax" is given. We
don't do this, because it's just some meta-masturbation.

But the idea is this: if the theory is sound, and if a statement is
true in every model of the theory, then that statement is provable.

**First Incompleteness Theorem**

Godel's Incompleteness Theorem says that any consistent system
contains a statement that cannot be proven. This follows from the
halting problem. The halting problem says you can't construct a
meta-TM that takes as input a TM description and an input and
determines whether the described machine halts on the given input.

If this existed, you could modify the machine to tell you whether an
input machine, _given its own description as input_, would
halt. Instead of returning true or false, you can further modify the
machine to run forever if the input machine would halt, and to stop if
the input machine would not halt.

But now give this machine its own description as input. Does it halt
or not? Paradox! Therefore, no such machine can exist.

To bring it back to Godel: we know that proofs are enumerable. It must
not be a solution to the halting problem for a TM to enumerate all
proofs, searching for either (a) a proof the machine halts given the
input or (b) a proof the input does not halt. If that worked, we'd
have a machine solve the halting problem. Therefore there are machines
where there is no proof whether they halt or not. That's Godel's First
Incompleteness Theorem.

What this is implying is that, in some models, this TM could halt, and
in others this TM could not halt. Basically: there's more than one
model for a theory.

By the way, this relies on the idea that we can encode the idea of a
turing machine into the formal language. That's one of the
requirements of Godel.

**Second Incompleteness Theorem**

Let us further consider the machine that tries to prove, given a TM
description and an input, whether the input machine will halt or run
forever. Such a machine, enumerating proofs will do one of three
things:

0. Return true when it finds a proof that the input halts.
1. Return false when it finds a proof that the input won't halt.
2. Runs forever, because no proof either way exists.

We do the same modification as before. First, modify the machine to
ask whether the input machine will halt when given its own description
as input. Then we construct a new machine which runs forever if the
input is proven to halt, stops if the input is proven not to halt, or
otherwise runs forever. That is:

0. Halt when we enumerate a proof that the input, when fed to
   itself, never halts.
1. Infinite loop when we enumerate a proof that input, when fed to
   itself, halts.
2. Or no proof either way is enumerated, in which case loop forever as
   in case 0.

Note that, unlike before, I am talking about a very specific
machine. _There is no question about whether this machine exists._

Feeding this machine to itself, it cannot halt. If it halted, then
that means it found a proof that it did not halt. Halting would
contradict the proof. That should mean that this machine either (1)
finds a proof it halts (in which case the formal system is clearly a
lie) or (2) there is no proof either way. Either way, it doesn't halt.

So I proved for you that the machine never halts. Oops, doesn't that
mean the machine should discover this "proof", in which case it does
halt? Doesn't that create a paradox?

How could we have arrived at a paradox? This is a real fucking machine
I made for you, damnit.

My proof that the machine cannot halt assumes that, if the machine
discovers a proof that it cannot halt, it is not allowed that it
violate that proof and halt. That's how I threw out the possibility
the machine halts.

But what if the formal system is a lie? Then, actually, our machine
might halt in defiance of the proof. That actually would be okay.

So my proof only applies if the formal system is consistent. If we
don't know the formal system is consistent, then the proof is not
actually valid, as it presumes something we don't know.

This brings us to the crux. Let's suppose the system _is consistent_,
since otherwise our "proof" above is just plain invalid. Then the
proof above is valid in formal system + axiom that formal system is
consistent. But this fact still doesn't make it valid in just the
formal system alone.

What if we could prove the consistency of the formal system? Then we
could write out the consistency proof as a lemma and tack on my
contentious proof from above to form a truly valid proof.

And that's what you can't do. You can't prove that the formal system
is consistent. Because otherwise the paradox really would
occur. Without that proof of consistency, the paradox does not occur.

Basically: incompleteness is saying that _one of things you can't
prove_ is _consistency itself_.

**Completeness Plus Incompleteness: Self-Hating Theory**

Incompleteness says that you can't prove consistency: that I can
accept. But Completeness says that there therefore must be a model of
the theory where the statement "the theory is inconsistent" is
_assigned true_. That means that, for a consistent system,
`A+Not(Consistent(A))` is also consistent (since `A` is certainly not
implying `Consistent(A)`)! This is called the _self-hating
theory_. What. The. Fuck.

**Summary**

Set theory and logic is wankery. Don't pay attention to it. We're
interested in new models of computation, not the foundations of
meta-mathematics. Kind of an interesting trip down memory lane, though
:-)

Another point: perhaps mathematics is not going to take us out of the
wilderness all by itself. We are crafting a mathematical model to a
physical system. There are many more choices to make. There's more
than one model consistent with the theory. As we observe new
phenomena, we may need to grow outr theory.

## Ch 4: Minds and Machines

Church-Turing thesis: anything computable is computable on a TM. This
isn't a proof of anything; it's more a guess. Will we come up with a
reasonable definition of "computable" that isn't covered by a
TM. Aaronson believes there aren't serious approaches suggesting
hypercomputation; that what we know about physics makes the idea of
hypercomputation unlikely (e.g., that the universe might be quantized,
not continuous).

Aaronson seems mostly to agree with me: that it seems like hubris to
believe that human brains are better or special in some way that AI
brains are not. He doesn't like Searle's Chinese Room, as he thinks it
isn't enlightening. In particular, he thinks that if we actually
started interacting with robots, we would start to treat them mostly
like other humans. Or rather: why do we think other people are
conscious? We know that _we_ are conscious, why grant that to other
people though? I guess the answer to that would be that we know that
we come from other humans...

He does wonder, as I do, about how it is that we "perceive"
ourselves. We don't "feel" like robots. If we were just robots, how
would we feel like we do?

## Ch 6: P, NP, and Friends

Naturally we're interested in things we can compute
efficiently. Efficient on what machine? We could say TM; then any
machine which (a) can efficiently simulate a TM and (b) be efficiently
simulated by a TM, has equivalent efficient computation power.

Even though Church-Turing hypothesizes that TM defines limit of
computability, there are possibly other machines that have more
powerful _efficiency_. That is probably where quantum computer will
come in.

Aaronson does ask why we choose polynomial as our standard of
tractability. He says that it's arbitrary, but works out
practically. He says that luckily, few algorithms are polynomial but
intractable. This is apparently called _Cobham's thesis_. I don't see
any particular deep justification, really, nor does Aaronson imply
one.

P is in PSPACE obviously. PSPACE is in EXP, because by the time you've
modified EXP memory cells, you're repeating a memory
configuration. You'd either be in an infinite loop or have terminated.

NP is in PSPACE (and thus EXP), as you can check all proofs of
polynomial length (remember that NP means there's a polysize proof you
can check in polytime; I guess the proof has to be polysize because
otherwise you couldn't even look at it all). And P is in NP of course
(because you could just ignore the proof and do the work yourself).

Note: `P=NP` is in some sense harder than other Clay problems: if
those problems have a solution, then it should be possible for a
computer to solve it in polytime. Though I'm not sure what that means,
since these problems aren't asymptotic in size.

I think he means this: proofs can be checked by a computer program; in
fact, it can be done in polytime. If `P=NP`, then it is possible to
produce proofs in polytime. What's the input size? I guess it's the
description of the problem. The decision problem is whether a proof
exists with under `n` symbols.

I guess that means, provided that there exists a relatively short
proof, we can "prove" this by running the decision machine if
`P=NP`. Of course, we have no idea of the constant factors. BTW: the
machine itself, and the history of its internal states, represents a
proof of the statement. And the length of such a proof is polynomial
in the input.

It's easy to construct an NP-Complete problem; take a polynomial-time
TM description; we want to know whether there is any input of length
`n` that causes the machine to accept. This is clearly NP, as we can
provide a proof in the form of an input that accepts, which can be
checked in polytime. Also, any NP problem can be converted to this.

3SAT is clearly in NP (you give me a setting of the vars). Note that
3Sat is an AND of OR statements about the variables (or their
negation). I think this is called 3-conjuctive-normal-form, BTW.

To show 3SAT is NP-complete, we convert to CircuitSAT. CircuitSAT is a
series of variable definitions equal to the AND, OR, or NOT of a
previous variable. Last var is the output; we want to know whether
there is a setting where the output is true.

We can write a CircuitSAT as a 3SAT. Basically, we need to convert
statements in CircuitSAT into ORs and NOTs that can be ANDed
together. For instance:

```
x_i = x_1 OR x_2
=>
AND(x_i OR not(x_1), x_i OR not(x_2), not(x_i) OR x_1 OR x_2)
```

Similar transformations apply to the other kinds of definitions. It's
polytime to do the transformation, and (of course) the input to SAT is
polysize, so if SAT could be solved in polytime, then CircuitSAT can
too.

But any NP problem can be written as a CircuitSAT. Basically, consider
a verifier for a given NP problem. Then you can basically write the
rules of the verifier as a CircuitSAT, describing each step. Claim is
this is a polynomial number of rules. So you've reduced your NP
problem to CircuitSAT, which reduces to 3SAT.

It turns out that if `P!=NP`, there are provably intermediate classes
of complexity between P and NP. But I certainly don't care.

coNP are those problems where a "no" answer can be checked in
polytime. Basically, `NP` is problems where a short proof of yes can
be checked, while `coNP` are problems where a short proof of no can be
checked. Just like P is a subset of NP, it is also a subset of coNP,
for the same reason (ignore the input, just solve the problem).

Even _given_ `P != NP`, we haven't been able to prove that `NP != coNP` (TODO1: though this is strongly suspected? Why?). OTOH, if
`P=NP`, then `NP=coNP` (or, equivalently, `P=coNP`).

This was unclear to me because I've defined `coNP` as problems where
you can check a counterexample efficiently. Another equivalent
definition is to say a problem is in `coNP` if it's "opposite problem"
is in NP. That is, if we can check rapidly a proof that the "opposite
problem" is true, then that is equivalent to saying the original
problem was false. So this would be equivalent to checking a
counterexample.

So, if `P=NP`, consider a problem in `coNP`; we know that the opposite
problem can be solved in polytime (since it is in NP). Therefore, by
`P=NP`, the original problem is also solved in polytime (because you
just solve the problem, then compute the opposite). Thus `coNP` is a
subset of `P`, and we already had that `P` is a subset of `coNP`.

The idea here is that `P` is _closed under complement_. Basically
`P=NP=coNP`. The question of whether `NP!=coNP` is whether _those_
sets are closed under complement.

Let us assume that `P!=NP`, as we expect. Is `NP!=coNP`: we strongly
suspect so. If our intuition is correct, then no NP-complete problem
can be in `coNP` (proof follows soon!). That is, problems where we can
quickly check proofs of yes _and_ check proofs of no are not expected
to be the hardest problems in `NP` (or, in `coNP` for that matter).

For a concrete example consider decision versions of factorization:
these are in the intersection of `NP` and `coNP`. Say I ask you about
a polytime calculable property of the prime factorization of an
input. Then providing the prime factorization allows me to verify or
disprove the statement. NB: you'd need to check that the provided
input really is a prime factorization: it's easy to test whether the
product is right, but to check the factors are prime you need AKS
primality testing.

So why would an NP-complete problem in the intersection of NP and coNP
imply `NP=coNP`? First, we immediately have `NP` as a subset of
`coNP`; we just need the other way. So take a problem in `coNP`; the
opposite problem is in `NP`. But that means the opposite problem is in
`coNP`! Therefore, the original problem must also be in `NP`. We could
do another proof to show that no `coNP`-complete problem can be in
`NP`.

So how hard are problems in the intersection of `NP` and `coNP`? We
don't even know that either; we haven't been able to prove that
`P!=NP∩coNP`, even assuming `P!=NP` (note this would further imply
`NP!=coNP`)! But we don't expect the intersection to be `P`; we expect
it to be intermediate between `P` and `NP`-complete (and
`coNP`-complete, for that matter).

Aaronson talks about polynomial hierarchy, which is a generalization
of NP and coNP, but I don't care too much right now...

## Ch 7: Randomness

Expectation of variables is linear: expectation of a sum of two
variables (even if correlated) is sum of their expectations.

Markov's inequality is `E[X>kE[X]]<1/k`. That makes sense, since if
the probability were higher, then `E[X]` would be too low (this
assumes X is always non-negative). TODO1: this implies Chernoff's
bound, which I'm too lazy to do right now.

Randomness matters in complexity because of randomized algorithms,
which produce a solution with a given accuracy.

He gives an example of starting at 1 and performing a series of
arithmetic operations (add/subtract/mult). The decision problem is
whether you get zero as an output. You could run the problem, but that
might produce large intermediate numbers. To do it in polytime, do it
mod a large prime. If the answer is not zero, then clearly the output
is not zero in the original problem. If it is zero, what's the
probability of being wrong?

It turns out that if you choose p large enough, some properties of
densities of primes suggests that the probability of mistakenly
getting zero as the output is zero probability (not interested why).

Interesting point: generating a prime number of length `n` has
probability `1/n` from the prime number theorem (maybe I've
misinterpreted; I don't understand the discrete math, and don't really
care). So you just keep generating random numbers of this length. You
can test in polytime whether it's a prime. So you can generate a prime
with probability 1 in polytime.

Amusingly, primality testing itself had to be done probabilistically.

There are a bunch of randomized algorithm classes. `PP` (probabilistic
polytime) is class of algorithms that terminate in polytime, where
acceptance if answer is yes is >0.5, and likewise rejection if answer
is no is >0.5.

We can repeat the algorithm multiple times to get a higher probability
of the correct answer. But imagine that the probability of correctly
answering yes or no is declining rapidly toward 0.5 as the problem
size increases: e.g., `0.5+2**-n`. Then to get a correct answer for
some prob `0.5+eps`, the number of trials needed will grow
exponentially!

Note that PP contains NP. Consider a boolean formula: accept
immediately with prob `0.5-2**-2n`. If not, try a random variable
setting, and accept if it works. If there is even one working variable
setting, you have a probability >=`2**-n` of picking it; thus, you
have >0.5 chance of accepting if there is such a setting. Likewise, if
there is no such setting, the probability of acceptance is exactly
`0.5-2**-2n`. So PP is a pretty _uninteresting_, large class. By a
symmetric argument, PP contains coNP.

Basically, PP contains problems where your discriminative power may be
decreasing too rapidly with problem size so that you cannot
efficiently solve large problems with high accuracy. So PP contains
problems more difficult than any in NP; but note that PP can only use
polyspace, so it still is contained in EXP.

BPP (bounded probabilistic polytime) is a more interesting class,
since it says that there exists an eps such that, if the answer is
yes, we accept with `>=0.5+eps` prob, and likewise if answer is no, we
reject with `>=0.5+eps` prob.

As before, we can use amplification to reduce the probability we will
fail to accept when the true answer is yes, or fail to reject when the
true answer is no. But, moreover, this amplification will hold even as
we scale the problem.

I want to make a note: the defintion of BPP has very little to do with
_false positive and false negative rates_. A problem where we can
correctly recognize a yes with high probability and correctly
recognize a no with high probability may still have a high false
negative (or false positive) rate if almost all answers are yes (or
no).

In fact, the false positive or false negative rate of a BPP solution
is unbounded in the limit. That's because this is in part a function
of the prior probability of the negative and positive classes. A note:
for a given problem size, via amplification we can acheive an
arbitrarily small false positive and false negative rate. It's only as
we scale problem size that the prior probabilities may change! That
may require more amplification to keep one or the other rate low.

The point is: in BPP our _total_ error rate can be made effectively
zero. BPP is one of the largest classes of problems we could consider
practicable with a computer based on a classical model of physics.

But sometimes you do want at least one side to be certain. So RP
(randomized polytime; why call it that??) is those problems where you
accept with prob greater than 0.5 if the answer is true, but reject
with prob zero if the answer is false. Basically: false negatives are
okay, but false positives are not allowed. There is an obvious
compliment: coRP, where false negatives are not allowed.

Both RP (and coRP) are in BPP. Why: can't the probability of correctly
recognizing a positive example approach 0.5 as before? Why don't we
need a bound as we did to separate PP and BPP? The reason is because a
problem is in PP and not BPP only if discriminative power goes to
zero; that is, we need the ability to recognize both positives and
negatives to approach 0.5. RP and coRP are both one sided, so the
discriminative power doesn't go to zero.

As before, false negative and false positive rates _can_ go to
zero. But overall error rate is bounded by any `eps` for a fixed
number of amplifications.

ZPP (zero-error probabilistic polytime) is the intersection of RP and
coRP. Another definition: these are the problems that you can always
get the right answer, but in _expected_ polytime. The first definition
implies the second, you need to call RP and coRP until you get an
answer you're sure of. But the number of necessary trials will fall
very rapidly.

A third definition: these are problems where, in exactly polytime, you
get the right answer at least 50% of the time, or "I don't know" up to
50% of the time. Again, this third definition implies the second.

The second definition clearly implies the third: you can just stop
early if needed. There must be some cutoff where the probability you
need more time will be less than 50%. This cutoff must also be
polynomial. I am currently too lazy to prove that the second
definition implies the first. Whatever...

We don't know whether NP (or coNP) contains BPP. That sort of makes
sense: BPP may tell us with arbitrarily high probability that
something is true, but that is _not_ a proof. We certainly don't
expect NP to be contained in BPP, since that would imply practical
solutions to all NP-complete problems (and, basically, all crypto
would be broken).

**Even More Complexity!**

There's this idea of _advice_. Advice is extremely powerful, changing
even what is computable. Advice is a function of input length. For
instance, `P/1` (polytime with O(1) advice) can solve a unary version
of the halting problem (the length of the input represents "the
nth-turing machine", the advice is whether it halts).

That seems odd, because the advice function should be construtable?
Does a halting problem advice function exist in ZF? Whatever.

`BPP` is provably contained within `P/poly` (Adleman); that is,
_non-uniformity_ (which is another name for advice) is at least as
strong as randomness. But we don't believe that `BQP` is contained in
`P/poly`, which suggests that BQP contains problems not efficiently
solvable by real computers.

But also: there appears to be growing proof that `P=BPP`. There's a
theorem by Impagliazzo-Wigderson strongly hinting this must be
true. Moving a problem from BPP to P is called _derandomization_.

Okay: I clearly don't follow a lot of this; it's above my paygrade
mabye. But I understand enough that I might be able to come back and
make more sense...

## Ch 8: Crypto

OTP is clearly optimal, but hard to distribute keys.

How about key stretching? That's a CSPRNG. What you want:

- A TM that:
  - Takes an `n` bit input.
  - Outputs a `p(n)` bit output.
  - Runs in polytime.
  - Call output `f(n)`
- For every other TM B:
  - `Prob(A accepts f(x)) - Pr(A accepts y) < \eps`
  - Where `x` is an `n` bit string and `y` is a `p(n)` length
    string.
  - Of course this needs to be in the limit.

It would be enough to be able to stretch input even 1 bit, since then
we could just keep applying this algorithm recursively to stretch as
much as we want, in polytime, and still have polytime security against
detection.

If this exists, we can make OTP encryption work with short keys.

Note that such a CSPRNG can only exist if `P!=NP`. If `P=NP`, we can
quickly decide whether some n-bit input is mapped to a `p(n)`-input
`y`. If a number `y` has an inverse under the CSPRNG, it is almost
certainly generated by the CSPRNG, and not true randomness. We could
distinguish samples of the CSPRNG from true randomness.

CSPRNGs can be based on problems that are considered to be
hard. Blum-Blum-Shub is one example based on factoring. But factoring
isn't hard in the quantum world. However, we have candidates for hard
problems in a quantum world that could be the basis of CSPRNG.

One reason it's hard to build CSPRNGs: it's hard to base them on
problems with _worst-case_ difficulty. We need _average-case_
difficulty. One such problem is "shortest vector problem" (who cares
what exactly), which is the basis of _lattice-based
cryptosystems_. But it looks like this problem isn't NP-complete,
thought it may be harder than polynomial!

Aaronson suggests that even if NP-complete problems might be "hard on
average", it doesn't seem obvious how to base a PRG on them.

Mentions OWFs. These have the property:

- A TM calculates `f(n)` in polytime.
- For all TMs A:
  - Assuming A is polytime.
  - `Pr(f(A(f(x))) = f(x)) < eps`.
  - Again, this is an asymptotic notion.

Basically, this is saying it's hard to find a pre-image. A PRG is a
OWF. Basically, if you could find a pre-image of `y` for a PRG, then
you'd know (almost surely) it came from the PRG, and not true
randomness. Turns out you can prove the other way: construct PRG from
a OWF. That's a hard proof.

Anyway, what we want is a trapdoor OWF so we can do public crypto:
public key cryptography and authentication/signing. RSA does
this. Aaronson notes that RSA has not been proven to be as hard as
factoring, though we can't think of a better way to do it. There is a
variant that _is_ as hard as factoring.

But factoring can be done efficiently by Shor's algorithm in a quantum
computer. That leads us to lattice based cryptography, which is secure
against quantum computers if its secure against classical
machines. Also, lattice crypto is a classical algorithm, so we don't
need quantum machines to do it.

Aside: homomorphic cryptosystems can be based on lattice
problems. This lets you compute on encrypted data.

Lattice cryptography is not yet efficient, but it is getting more
practical.

## Ch 9: Quantum

Quantum generalizes probability to include negative and imaginary
amplitudes. Okay, take a bit. It has a certain probability of being a
zero or one: `p` and `1-p`. But let's now say that probabilities could
be `alpha` and `beta`, each imaginary numbers (we call these
_amplitudes_). In fact, for now, let's keep alpha and beta to be real
numbers.

We still need the total probability to "sum to one": we'll require
`alpha**2+beta**2=1`. In fact, if you observe the bit, you will see
either a one or zero with probabilities `alpha` and `beta`.

It seems pointless to use alpha and beta, instead of their
squares. But what matters are the _transformations_ that we can apply
to the system. Transformations to classical probability vectors are
_stochastic matrices_: those where the columns add to one. We write
`Ax`, where `x` is a column vector of probabilities. Each column of
`A` describes the probability that, if `x` were `i` before, it will
transition to `j` with probability `A_{j,i}`. Of course, if `x=i`, it
must transition somewhere, so the columns sum to unity.

Stochastic matrices are _exactly_ those that map probability vectors
to probability vectors. In more mathy words: they are the matrices
that preserve 1-norm.

So, given an amplitude vector with 2-norm 1, what matrices will map it
to another vector of 2-norm 1? When we're talking real numbers, the
_orthonormal matrices_ are the ones that do this. When the vector
space is over the complex numbers, these will be the _unitary
matrices_.

I know a fair bit about orthogonal matrices from linear algebra; I
know less about unitary matrices. For instance, orthogonal matrices
are those where the inverse is equal to the transpose. Orthonormal
matrices are the only matrices that can preserve 2-norm, because even
if basis vectors were mapped vectors of unit length, but not
orthogonal, then this would reduce the angle between those two
components. What that effectively is doing is putting a little bit of
one component parallel to another. So we're shifting around lengths of
the vectors in the original basis. But if we make one vector shorter
and another longer, that typically won't preserve equality of the
2-norm. Blah blah blah.

What we've seen here so far is a generalization from positive real
probabilities with 1-norm to real numbers with 2-norm.

Ket notation would write a vector as `a|0> + b|1>`, which is weird,
but is just physicist's way of writing a vector. This is Dirac _ket
notation_.

These matrices let us speak about _quantum interference_. Take the
matrix:

```
1/sqrt(2), -1/sqrt(2)
1/sqrt(2), 1/sqrt(2)
```

Note that this is an orthonormal matrix.

Applying this to a `1|0>`, apply a second time. You'll get
`1|1>`. How? We performed a randomizing operation twice, but ended up
with a deterministic outcome! You might say there are two paths to
`1|1>`: one path through `|0>`, and another through `|1>`. But the two
paths _destructively interfered_ to cancel out the possibility of
`0|0>` and _constructively interfered_ to make `1|1>` 100% probable.

Let us say that we do not know which of two quantum states we are
in. Let's say there is a 50/50 chance of `1/sqrt(2)(|0>+|1>)` and
`1/sqrt(2)(|0>-|1>)`. For each vector, we take its outer product with
itself: this is called a _density matrix_. We then take the linear
combination of the density matrices, weighted by the appropriate
probability. In our case, we have:

```
0.5 0
0 0.5
```

This describes the _mixed state_. Note that it is identical to the
density matrix for a 50/50 probability of `|0>` and `|1>`. In fact,
there is no experiment that will distinguish these two mixed states;
they are effectively identical.

Okay, some arguments about why this is natural. First, in order to get
an interesting space of norm-preserving linear transformations, we
need to either (1) use the 1-norm or (2) use the 2-norm. So in some
sense, 2-norm is the only alternative that makes sense. Aaronson gives
some other arguments.

Second: why complex numbers? Well, if you want to be able to always
divde a linear transformation into two steps, you need complex
matrices. For instance, take a mirror reveral of the plane:

```
1  0
0 -1
```

To divide this "in half" you need to take its square root. Then you
apply the transformation twice. For the norm to be preserved
throughout, you need another dimension to travel in, which is what the
complex numbers are giving you.

So basically, complex numbers are required if you want _continuity_ of
transformations.

Now, if you have two qubits and want to consider them together, you
typically take the tensor product of their states. This is basically:

    (a|0>+b|1>)(c|0>+d|1>)=ac|00>+ad|01>+bc|10>+bd|11>

That's basically what you expect. However, there are some arrangements
which _cannot_ be a tensor product:

    (|00>+|11>)/sqrt(2)

This is a pair of _entangled_ qubits. We might say that there state is
not independent.

He goes on to discuss no cloning, quantum crypto, and quantum
teleportation. But I've covered all those in my quantum subfolder. I
augmented Aaronson with the Quantum Computing Without The Physics paper.

TODO1: I'm not sure I really understand mixed state.

## Ch 10: Quantum Computing

He talks about universal gate set. He talks about how, if you want to
use a quantum algo as a subroutine of your quantum algo, you may have to
"uncompute" so that you "erase" your Turing tape so that you're all
zeroes again. That matters because you may want to interfere the results
of your subroutines, and you can't have the subroutine results entangled
with subroutine state else to do that.

He says that BPP is in BQP. It's because anywhere you need to flip a
coin, just apply Hadamard to `|0>` and observe.

Of course, BQP is in EXP, because it can be simulated.

He mentions that BQP is in PP. Here's why. He notes that to compute the
probability that a machine accepts, you can iterate all computational
paths. That is, for each gate, you pick one direction to go in. You
accumulate a single amplitude along the way.

Of course, there are so many paths. Some of the amplitudes are positive
and others are negative. But ultimately each is computable in polytime.

He already showed that whether the sum of exponentionally many terms is
greater than a given threshold can be solved in PP. Anyway, I don't need
to know everything about this...

He shows some algos like Simon's algorithm. Again, I have explored those
in my quantum folder. He notes that Simon's algorithm shows that,
relative to an oracle, `BPP != BQP`. That suggests that maybe BQP is
more powerful than BPP.

He does start to mention the many worlds explanation.

## Ch 11: Penrose

Penrose is trying to say that the human brain operates in a
nonalgorithmic way. He grounds this in Lucas' argument: if we work in
fixed formal system `F`, we can "see" that "This sentence cannot be
proved in F" is true, which shows that we aren't really working in
`F`.

Aaronson says: Lucas' argument is equivalent to claiming humans can
"see" that `F` is consistent. But can all humans truly "see" that? And
if we allow humans to be fallible in this assumption, then we should
grant the same option to be fallible to computers, in which case they
can "prove" the statement too.

Note: Penrose isn't saying the brain is a quantum computer. Quantum
computers still have the same computability as Turing
machines. Penrose is arguing that the brain can do hypercomputation.

If you have this claim about hypercomputation, you shouldn't even be
able to _simulate_ intelligence with a Turing machine. But Aaronson
says: surely you can do that with a huge lookup table.

So Aaronson says: I think what we want is a machine that can simulate
intelligence _efficiently_. Then it may be the case that no Turing
machine can even simulate intelligence (efficiently).

Of course, even if the brain is the kind of hypercomputer that can
"simulate" intelligence efficiently, then why does that make us think
that kind of computer is conscious. How do those things relate? And
anyway, what does that have to do with the Lucas argument now?

More generally: is there any relationship between quantum and
consciousness? He quips: "they're both mysterious so the answer must
be yes!" But, more charitably, he says: they both seem to have this
aspect of reconciling third-person reality with first-person
experience.

Aaronson gives some reasons why they might not be related. If a
non-quantum computer can be reproductively successful, why would it be
selected for? People don't seem too be doing anything obvious that is
only efficient on a quantum machine. The brain seems a weird place for
quantum coherence to last. And anyway: all this speculation is
supposed to explain consciousness, but it would still be equally
mysterious why a computer computer has consciousness.

He suggests one positive thing, though. Imagine the transporter
experiment. Why might you not be okay with being destroyed? Perhaps
the brain stores "you" inside quantum state. Then that can't be
cloned. So the "transporter" cannot be duplicating you; it either
fails to copy the quantum state, OR it would need to work in a fancier
way. It would need to deconstruct you _as_ you were reassembled on the
other side (a la quantum teleportation).

## Ch 12: Decoherence And Hidden Variables

Notes that the weirdness of quantum is not that it is stochastic, but
that there is interference. Says otherwise Einstein would not think this
is spooky (just that we might have imprecise measurement).

So he starts out: what if your brain is in a superposition of seeing a
red or blue dot. Then we do a unitary transformation. What's the
probability of seeing a red dot given that you saw a blue dot before?

Basically: you can't do "conditioning" when talking about quantum
states. At a previous time, you weren't in any one state: you were in a
superposition. And if you had measured at the prior time, then running
the experiment would give you a totally different result.

This challenges the idea that you have a "past."

Anyway, he talks about decoherence. Basically, say you have an EPR pair,
but you lose one of the qubits. Now you don't get the proper
interference when you apply quantum operations to the remaining qubit.

He mentions that he thinks that (1) decoherence is a lot like the second
law (because as system decoheres, any individual region of space will
see its entropy go up). He also suggests that (2) this is what motivates
the arrow of time. As in: as time moves forward, the system decoheres,
and branches of the quantum state will separate and probably never
interfere again in the "future."

He notes that the tree can't branch forever (even in an infinite
dimensional space?). But he notes that we'll all be dead by the time the
branches start interfering with each other. Anyway, the whole discussion
kind of dodges the original question about "conditioning" on past
experience.

**Hidden Variables**

Basically, a hidden variable theory tries to give an explanation for any
quantum phenomenon in terms of operations on a normal L1 probability
state. He notes that by definition they are indistinguishable from
quantum theory.

He notes 4 things about hidden variable theories:

- If you want to cook up a transition matrix that evolves the
  probabilistic state to match the evolution of quantum state `\phi`
  with unitary operation `U` applied, then the transition matrix must
  also depend on the state `\phi`. That is: the evolution function for
  the hidden variable state is not merely a translation of the evolution
  function for the quantum state.
- Any hidden variables theory will not give the same stochastic
  transition matrix for `VU` as you get from translating `V` and `U`
  independently and multiplying. So the translation is not "time-slice
  independent."
- Skipped one...
- Last, Bell's theorem shows that any hidden variable theory needs to
  have an explanation involving "instantaneous communication." He
  stresses: not that we could use the hidden variable theory to send
  messages faster than light; just that there needs to be some kind of
  collapse across points instantaneously.

He gives some hidden variable theories. He ends with the Bohmian one.
Basically, Bohmian mechanics is _deterministic_, but non-local. The
transition matrix is all ones and zeroes.

Anyway... I don't know that I fully understand these things...

## Ch 13: Proofs

He investigates probabilistic proofs. One class is `MA`: Merlin-Arthur.
Here, the question is: "Are there polynomial sized proofs that can be
verified by a randomized algorithm?" This is a smashup of NP with BPP.
This is kind of "probabilistically verifiable proof." A language `L` is
in `MA` if (1) any `x \in L` admits a proof `w` that can be verified
with certainty by a verifier algorithm `V(x, w)`, (2) if `x \not\in L`,
then for every fake proof `w`, we should have `V(x, w)` returns false
with probability at least 50% no matter the choice of `w`.

Basically: you can verify a true proof that `x \in L`, and if you do
enough rounds of checking, you will be probabilistically sure to reject
a false proof that `x \in L`. Of course, we haven't talked about proofs
that `x \not\in L`: that sounds like `co-MA`.

Some problems might not be in `MA` if Merlin has a tricky way of
generating false proofs that can't be detected. Thus, we can imagine
first issuing a random challenge to Merlin. Merlin's proof must be
responsive to this challenge. This is called Arthur-Merlin `AM`. `AM`
should contain `MA`, which is just the version with no challenge.

(Aaronson doesn't precisely define `MA`. `MA` may be defined such that
Arthur should verify deterministically, which seems less powerful. But
there appears to be a result of Goldwasser/Sipser that says that
"private" randomness is not needed.)

Aaronson notes that it is strongly presume that `AM` is in fact exactly
equal to `MA`, not strictly stronger. It is further expected that they
are both equal to `NP`, which would correspond with `P=BPP`.

**Zero Knowledge Proofs**

He talks about zero-knowledge proofs. One example is graph
non-isomorphism. Say you want to prove that you know two graphs are
non-isomorphic. Then you let the challenger submit a permutation of one
of the two graphs. Because they are not isomorphic, you can tell which
one this corresponds to. You reply thusly. This convinces the
challenger.

This seems intuitively "zero knowledge:" the challenger "learns" nothing
here. But can we formalize this?

First, even though the verifier now believes that the two graphs are not
isomorphic, they should not be able to prove this onward to anyone else.
They could show the transcript to someone new, but this is unconvincing,
because how does the third party know that the prover and the verifier
weren't colluding?

In particular: how would a third party know that the transcript wasn't
simply something the alleged "verifier" wrote all by themself?

Basically: the proof will be non-convincing if there is a simulator
program that would generate fake transcripts with the same probability
distribution as true transcripts. This is called _statistical ZKP_.

This captures the idea of the original proof protocol being zero
knowledge, because it shows that a sample from the true proof protocol
transcript says nothing, since that was equally probable to sample from
the simulator transcript space.

Another kind is a _computational ZKP_. Here, we do our graph coloring
thing, where we encrypt our colorings for commitment. We are relying on
non-invertibility of our encryption. Clearly `SZK` is contained in
`CZK`. All NP-complete can be encoded as graph coloring, so all
NP-complete problems have a computational ZKP.

A _probabilistically checkable proof_ (PCP) says that you can verify a
proof by just looking in a few random places. That is: in the right
proof system, a false proof should make errors just about everywhere.

Consider graph isomorphism. For problem size `n`, consider an
enumeration of all graphs of size `n`. The "proof" is an exponentially
long string, where each bit `i` corresponds to the `i`th graph in the
list. You give a zero if the `i`th graph corresponds to `G_0` or a one
if it corresponds to `G_1`. Else you choose randomly (doesn't matter).

The checker chooses a graph, does a permutation, and then checks that
position to see if you have a zero or one there. They quickly realize
that you have always got the right answer.

Of course, this is bogus because a huge string is required. But the PCP
theorem says (1) every NP problem has a PCP, and (2) the proof length is
polynomial in the problem size.

Last, he explores a complexity class called DQP. Basically, this is a
model of computation which is like quantum, but it assumes a hidden
variable theory, and it says that when you measure, you don't just get
the result, but you get the whole history of the hidden variables.

He shows how this would give you some speedup over BQP on a certain kind
of problem. He doesn't believe this kind of computing device exists, but
he thinks it is interesting because it shows that you can "modestly"
extend BQP without being able to solve all NP complete problems.

## More

- An interview here:

http://blogs.scientificamerican.com/cross-check/scott-aaronson-answers-every-ridiculously-big-question-i-throw-at-him/
