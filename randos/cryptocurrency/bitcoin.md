## Summary

Bitcoin addresses are basically hashes of public keys. To spend money
at an address, you need to produce the public key and do a digital
signature. The reason the address is a hash of a public key is so
people can't start trying to crack the private key. You provide a
destination address, plus possibly an offer of a transaction fee.

It is typical to spend *all* the money at this by specifying a
destinaton for the *change* (the difference between the amount at the
spender's address and the amount sent to the recipient). That way,
even though you've revealed the public key when spending, you have
spent all the bitcoin at this address, ensuring that even if someone
now cracks the private key, there's nothing left to spend.

A recipient of such a promise needs to know you haven't already
promised the coin to someone else: that you still own the coin. For
this reason, a ledger is necessary. The system would be simple with a
centralized ledger, but the innovation of Bitcoin is to decentralize
it. So we have a swarm where nodes maintain a ledger; clients
broadcast a transaction to the swarm so the nodes can add it to the
ledger.

Imagine a swarm where everyone was allowed to keep their own copy of
the ledger. There would be no consistent transaction ordering; it
would be chaos. The solution is to use the *longest ledger*; that a
ledger with a longer history is considered to be more authoritative
than a ledger with a shorter history. Note that this is a partial
ordering; different ledgers with equal length are equally valid. So we
can't be done yet.

We want the swarm to eventually come to a finalized, consensus opinion
on the ordering of transactions. For that reason, nodes occasionally
communicate their ledgers to each other. Nodes will abandon their copy
of the ledger when they are notified of a longer ledger kept by
another node.

To avoid unnecessary churn in the network, nodes batch transactions
into *blocks*. The node waits for a random period of time before it
adds the block and notifies other nodes in the swarm. The waiting
period should be (on average) significantly greater than the latency
in the network. The hope is that, once a block is committed by one
node, it is unlikely that another node will try to add its own version
of this block before it is notified.

The older a block is (the more subsequent blocks have been added), the
less likely it is to be winnowed. For an alternative chain to take
over, there would have to exist another chain (a) of greater length
and (b) that chain must have failed to be in communication with us for
a long period (proportionate to the number of discarded blocks), as
otherwise someone would previously have informed the other of their
superiority.

Note that the most recent block could be fairly subject to winnowing
(for instance, two nodes approximately simultaenously tried to add a
block). But that should heal relatively soon. Still, the partition, as
long as it exists, is an unfortunate circumstance. Clients who can
only communicate with one side of the partition may rely on that
side's transaction history, which will later get nuked when the
partition heals.

Which side of the partition will win? If clients can communicate with
both sides of the partition, can they guess which version of the
history is likely to be chosen?

In general, the larger side of the partition will win, since more
nodes means that blocks are added faster. This brings up another
point: the time a node waits to add a block should be dynamically
adjusted so that blocks aren't added too quickly as the swarm expands
in size. Note that, in the long run, both halves of the partition will
be adding blocks at the same rate, so you measure the longer chain
weighted by the sleep time.

Okay okay okay. The system generally works in the absence of malicious
users, but how do we tolerate them? Malicious users can't forge
transactions (because of the signing), but they could try to modify
the transaction history. In particular, the malicious users could
disregard the global rate at which blocks are supposed to be produced,
so that they can speed ahead and add blocks. In that way, they could
rewrite blocks that we thought were well-established history.

The solution is to enforce the sleeping by a proof-of-work. A typical
proof of work says: find a pre-image of a hashing function less than a
given value. The magnitude of this value is what needs to vary over
time so that blocks continue to be produced at approximately the
correct rate.

So attackers can't game the system adding a bunch of blocks to rewrite
history. Also, to prevent them from modifying past history, each block
includes the cryptographic hash of the previous block. If an attacker
tries to modify a prior block, he will change this hash; future blocks
which contain that hash will not be valid successors of the attacker's
changed block. They will have a short chain.

What stops an attacker from computing and storing a bunch of hash
pre-images, then dropping a bunch of blocks to rewrite history? This
is prevented because we require that the "pre-image" needs to be xored
with the hash of the previous block before hashing, ensuring we cannot
begin calculating a pre-image until the previous block has been
commited.

Okay, in actual fact, there is this thing called the block header; it
contains the previous block header's hash, a hash of the transactions
in the block, a block completion time, and a target work value, and a
*nonce*. The nonce is the only free variable here. We try different
nonces until the hash of the block header is less than the target work
value.

Note that the target work value is a deterministic calculation from
previous block completion times so that the difficulty can be adjusted
adaptively.

An attacker, then, can't go back and change history without producing
new blocks to the succeeding true history. But the attacker has to
spend computational resources to produce the alternate history. But
when there are many participants, the amount of work required is very
high. For an adversary to rewrite the most recent block, it must find
an alternative block, *and* produce the next block. As the adversary
tries to rewrite blocks further and further back, this requires more
and more CPU resource.

And while the adversary is trying to rewrite history, the swarm is
extending the history, so to avoid falling behind, the adversary must
maintain a superior amount of CPU power to ensure that it can win the
race to produce the next blocks and outpace the good guys.

By design, we can say things like "A malicious user with X% of the
network's CPU has a Y% chance of rewriting a block Z blocks old".

Why should anyone participate in the swarm? Members of the swarm,
called *miners* receive a reward for producing blocks: this
incentivizes their CPU work spent producing blocks. They also receive
transaction fees; miners may ignore transactions if they are not given
a transaction fee.

Bitcoin distributes the currency by rewards for mining; that solves
the initial distribution of the currency. This is probably a good
social hack to encourage people to participate in keeping the
transaction history, but it is not strictly necessary from a
distributed systems point of view. Likewise, Bitcoin reduces the
reward for new blocks over time, so that the number of bitcoins
asymptotically approach a maximum.

The blockchain itself is the most interesting part, from a CS point of
view: the idea of a distributed transaction history. The crypto means
that transactions need to be properly authorized. The absence of
personal information gives a pseudonymous aspect to the system.

But I suppose that the blockchain couldn't exist without the currency,
because otherwise there is no incentive to mine, which means the
blockchain would be vulnerable to attack.

The blockchain can be used for other purposes. For instance, Namecoin
is a distributed DNS registry system.

## An Alternative That Doesn't Work

Why don't we just ask the network for several copies of the
blockchain, and pick which one is returned most often? That seems like
the majority believes in that chain. The reason is a Sybil attack,
where an attacker pretends to be a lot of people. Only by proof of
work can we see how many people vote for a blockchain.

## Multisig Transactions

Bitcoin has multi-signature addresses so that multiple people are
required to spend a bitcoin. It can even do m-of-n transactions.

One nuisance: what if I sign a contract, but you just sit on it and
hold on to it without signing. You can drop it on me at any time. I
guess we'd specify a time by which the contract neesd to be signed...

## Transaction Commit Verification: Thick-Wallet

To verify a transaction has been commited, a client could request
copies of the blockchain from several machines in the network. If it
gets different copies of the blockchain, it chooses the longest. The
hope is that, even in the presence of a Sybil attack, only one copy of
the true blockchain needs to be found by the user.

The user can then go back through the blocks of the longest chain it
finds, looking for the transaction. We assume it can locate the
transaction.

What is the probability that this block is later forked away? If an
adversary is competing with the rest of the network to add blocks to a
target block, the question is with what probability can the adversary
overtake the network?

This depends on the CPU power of the attacker. If it is >50% of the
network, the attacker can, given enough time, produce more blocks than
the rest of the network. The deeper the block is in the history, the
longer the attacker will have to maintain this advantage.

Adversaries with low CPU power will, in general, be outpaced by the
network. However, due to randomness, there is a chance an attacker
will produce blocks more rapidly than the network for a short period
of time. The idea is that, the more blocks the network has produced
after the target, the less likely the attacker will be able to produce
faster than the network long enough to get ahead.

NB: if a client cannot find any valid copies of the blockchain, they
can be fooled by a Sybil attack...

## A Twist: Saving Space

Storing the entire transaction history is a little much. Once an
address exhausts its balance, there is no need to store the
transaction that funded that address. You would like to delete these
irrelevant transactions.

However, if you do that, you will lose the ability to prove that other
transactions are part of the block. That's because the hash of the
block's entire transaction list is stored in the block header.

To facilitate this posibility, a Merkle root is stored in the
header. You can garbage collect transactions by cutting leaves and
replacing with their parent hash. You can provide the tree, which is
still authenticated by the Merkle root.

Note that by doing this kind of garbage collection, you would lose the
ability to prove that the transaction history is truly valid. The user
would have to look to the depth of a block as a proxy for its
validity: the network at one point believed this was a valid block and
kept building on it.

## Transaction Commit Verification: Thin-Wallet, SPV

You can keep the entire blockchain history, which is somewhat
obnoxious. However, this allows you to truly check all the work
yourself; an adversary cannot fool you. In particular, you can search
for a single transaction.

Say you want to keep only the blockchain headers. If you want to check
that a transaction is processed, you might ask what block it is
included in. You can then download the transaction history of this
single block. However, note that you no longer can ensure for yourself
that this transaction was valid (though of course it must be signed
properly). However, further blocks that come after atest that the rest
of the network thought it was valid. Basically, you're trusting the
network a little bit more.

You don't really need the entire block; you can just ask for the
Merkle path to the transaction.

However, by asking about particular transactions, you are leaking
information to the network about what addresses you migth control.

## Replace-by-fee

When a transaction is in the "mempool" of a node, you are allowed to
send a replacement transaction. Why would you want to do this?

The idea is that, if you have not offered a high enough fee, your
transaction will sit unprocessed for a long time. But it *can* be
eventually processed, since it is valid.

You can replace a transaction, increasing the fee amount offered.

Downside: makes zero-conf verification (verification with no blocks
seen) impossible. There are some strategies like "first-seen-safe";
that lets you change the fee but not the addresses you send to.

## Blocksize

Presumably existed to prevent race to the bottom in transaction fees,
to impose some scarcity so mining was profitable. Also prevents
spamming of blockchain, which is bloat. But to increase transaction
rate, need to raise this.

There are other hurdles for scalability: signature verification,
network usage, storage space. I don't know enough to say whether the
blocksize is the major impediment to growth. Some people say
blockchain won't scale to Visa size, so don't try to push it too far.

They also worry that requiring more computational resources to mine is
going to put small guys out of business, causing centralization.

I think it's hard to say. There seems like a tragedy of the commons
here: it's relatively costless to validate more transactions, but then
miners don't make any money.

https://en.bitcoin.it/wiki/Scalability
https://en.bitcoin.it/wiki/Block_size_limit_controversy

## Bitcoin Economics

Bitcoin mining uses a lot of resources: at least $150k worth of energy
each day. The utility of this proof of work is that it makes it very
expensive for an attacker to produce long chains to rewrite
history. To mine blocks, they have to expend a real world
resource. Importantly: if their attack doesn't succeed, this
expenditure was in vain.

Another problem is the economics of mining. Mining blocks is
expensive, but the marginal cost of adding a transaction to a block is
effectively zero (assuming the blocksize increases). In most markets,
that should lead to natural monopoly.

Let's think more about this. If a miner controls ~0% of the hash
power, it is in their interest to accept transactions at any non-zero
fee. OTOH, if a miner controls 100% of the hash power, every customer
who *would* want to transact at the price the miner quotes will *be
required* to pay the quoted price. The miner with 100% power has full
price-setting power, while the miner with ~0% of the power must accept
an arbitrarily small fee.

Let us consider an intermediate case: a miner controls 50% of the
hashing power. If they raise their asking price, they will lose
customers. In particular, they will lose customers who are not willing
to pay the difference between the miner's higher rate and the
prevailing rate of the other 50% of hashers in order to guarantee
their transaction is included in the next block. The geometric
distribution says that if a miner controls `p` of the hashing power,
the user will have to wait `1/(1-p)` blocks for someone else to accept
the transaction: in the case of 50% of the hash power, 2 blocks.

For most use cases, I don't imagine people would be very likely at all
to pay more to get their transaction processed 10min sooner. This is
especially true because most transactions are either (1) very rapid,
like buying coffee at a store or (2) very long, like shipping a
product. I don't forsee many cases where there is a marginal benefit
to having a transaction processed in two blocks rather than one.

50% hashing power is a very extreme case; any more and the system has
effectively become captured by a single party. So we can see that,
even in the most extreme cases where bitcoin is possibly functioning,
bitcoin miners have very little ability to set the price higher than
the currently prevailing rate.

Miners have incentive to accept a lower transaction fee than the
prevailing rate, if the prevailing rate is higher than their marginal
cost to hash an extra transaction. Adding additional transactions for
this block does not reduce the fee the miner receives on other, higher
fee transactions.

How easy would it be for cartels to form? Cartels could try to
organize to enforce discipline on fees, but there would of course be
incentive to defect. In fact, the pseudonymous nature of bitcoin makes
it particularly difficult to monitor defection!

Also: bitcoin mining equipment is becoming more and more
significant. Higher fixed costs in the presence of very low marginal
costs pushes us toward natural monopoly.

It is therefore quite likely that bitcoin mining would be captured by
a single party. But that would give the party control, which defeats
the fundamental premise of Bitcoin. What is the utility of single
party control?

Therefore: **how can we avoid centralization of bitcoin, and how can
we avoid wasting energy?**

Here's a blog post also talking about the possibility:

http://hackingdistributed.com/2014/06/16/how-a-mining-monopoly-can-attack-bitcoin/?print=yes

Another source:

http://insidebitcoins.com/news/the-economics-of-bitcoin-mining-centralization/31833

And:

https://www.reddit.com/r/Bitcoin/comments/3y273t/eb110_james_dangelo_satoshis_big_mistake_and_the/

They claim that even if fees don't fall to zero, then economies of
scale will cause centralization of blockchain.

## Decentralization

I think true decentralization will only happen when the people
protecting the network are basically just doing it for fun. Otherwise,
there is too much incentive to centralize.

But the problem is that computing power is what keeps the blockchain
secure. E.g., if the blockchain people said no transaction fees and no
block rewards would be honored, that would mean mining wouldn't be
profitable, but it would keep hashing power lower, and susceptible to
attack by an adversary...

## Altcoins

Top 10:
* Ripple: Don't understand???
* Litecoin: Mostly the same, GPU focused.
* Ethereum: Scripting.
* Dash: Previously Darkcoin, anonymity.
* Dogecoin: Meme bitcoin.
* Factom: Appears to store timestamped hashes of your documents?
* Peercoin: Proof of stake.
* NXT: Proof of stake.
* YBCoin: Chinese Bitcoin.
* BitShares: Proof of stake

Smaller:
* Namecoin: DNS.

## TODO3

* Ethereum: for contracts
