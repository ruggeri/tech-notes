## Proof of work

You want there to be a single authorative copy of the database in the
face of malicious actors.

So you have this chain of DB transactions. Everyone in the network
will agree that the longest chain of transaction blocks is the
authoritative one.

You don't want people to modify a transaction block (for instance, by
removing a spend), and then building on top of that, presenting this
as the truth.

First, you link the blocks together via hashing; this means that
someone can't modify a block in the past without having to redo blocks
that came after.

And you make it hard to remake those blocks, because it requires
hashing power.

Therefore, rewriting old blocks is very hard, because you'll have to
"catch up" and surpass to the authoratative chain, where most of the
miners are working.

Miners don't have any incentive to hash for you, since that wastes
time from their work on the authoratative chain.

## Proof of Stake

I believe the idea is this. First, there are validators. The
validators each sign blocks one after the other.

To keep there being one history, we must disincentivize signing two
blocks. The way that *slasher* does this is like so. If Markov signs
versions A and B of block 100, first note that validator Curie who
comes next will only build on one of the versions (because she doesn't
want to get smacked down like Markov is about to). Let's say Curie
builds on top of version A.

Anyone can submit a copy of Markov's signed version B to the chain
built on top of version A. This special transaction does not
invalidate Markov's version A on the A chain (that would revert
history), but it does steal Markov's stake that he has put up to
become a validator.

If Markov's stake to steal is 10 BTC, he can't pay every validator 10
BTC to ignore his malfeasance; that would not be economical. He
*could* pay the next validator, but malfeasance proof can be submitted
any time.

## Proof-of-Work Override

What if a validator is asleep? How will you skip them?

You can have a proof-of-work override. There's a problem that is
expected to take maybe 10min. If you solve the problem, you get to
mine the block. The next validator gets to sign in the normal way.

One question is: what stops you from just mining over-and-over to
rewrite history?

One possibility: the mining difficulty is different for different
successors. For instance, maybe whenever it is Markov's turn, then
Curie can skip with a difficulty that means 10min, but Gizmo can skip
with a difficulty that implies 20min.

The idea here is that while *someone* can skip relatively quickly,
it's not like that same person can just go to the past and mine
block-after-block via skipping.

## "Long Range Attack"?

(Not sure if I'm using the right term).

Will validators ever be able to "cash out?" What if they want to stop
being validators?

If validators cash out (or if they are tired and willing to lose their
stake), then they no longer have anything at stake. Which means they
can sell their old keys, and people can sign duplicate copies of old
blocks without any real penalty.

You can reduce the problem by enforcing a delay of 3k blocks before a
cash out is completed. This means that duplicate signed blocks should
always be at least 3k blocks old.

Except not really. Let's say that you wait until the entire set of old
validators has turned over. You buy *all* their keys, so you own 100%
of the validators. You can now mint blocks super fast to catch up
(because you can always mint as the next validator, who has the
easiest target, and you can just throw more CPUs at it).

One way to stop this is never allow a reorganization greater than 3k
blocks back. But that doesn't help anything, actually, for the same
reason it wouldn't with Bitcoin. Anyone new to the network would be
like: "WTF, why are you people working on this shorter version of the
chain?" So things would look bizarre.

## Thoughts

This sounds like you're trying to solve BFT, but with an incentive
scheme.
