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

Of course, this is highly energy inefficient.

What if we let anyone publish a block whenever they want? Then people
would be presenting lots of chains, and who could say which is better
than the others?

One way to prevent people from building on multiple chains is called
*slasher*. With slasher, it is considered misbehavior to forge blocks
on multiple chains. If Alice forges blocks on chains A and B, then Bob
can take her signature of the chain A block, and show that to chain
B. Chain B will forfeit her coins. Likewise, Bob can show chain A to
forfeit her coins.

This means there *is* a cost to building on both chains: forfeiture.

Okay, maybe people don't want to build on two chains. But why should
they work on the *same* chain?

One idea is if the group agrees on who should forge the next
block. Anyone who tries to skip this order can be ignored.

However: what if someone is "asleep"? Clients can be configured to
time out the validator, and move on to a "next in line"
validator. They now get the chance to forge.

People can try to "line jump:" publish a new block without waiting for
validator who is supposed to come next. The question is: will
validators work on top of your block? You can disincentivize this
through slasher; building on the "wrong" chain brings penalties. And
if you can only choose one, why not play the Keynsian beauty contest
of trying to guess who the others will pick?

One problem: harder to discover what is the authoritative history
now. Can no longer just judge on length.

## Thoughts

This sounds like you're trying to solve BFT, but with an incentive
scheme.
