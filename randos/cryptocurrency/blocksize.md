If blocks are mined at a constant rate of ~10min (because of adaptive
difficulty), then an upper bound on the limit of TPS is how many
transactions are allowed in one block.

Some Bitcoiners want to increase the block size to raise TPS. In
addition to raising TPS (so that more txs can be done on Bitcoin), you
would also have lower fees per transaction. That means it is
worthwhile to transact with lower amounts of currency. Lower fees
increase competitiveness with Visa.

## Shorter block time

Presumably the reason not to have shorter time between blocks (block
time) is because there is latency in the network, and we don't want to
orphan block unnecessarily. Orphaned blocks aren't just bad for
confirmation: they're wasted effort in that CPU work is supposed to
secure the chain, and CPU work spent on anything that isn't the main
chain is securing nothing important.

Ethereum has a block interval of 7sec vs 10min. One way this happens
is because they actually have a "block tree." Here, the weight of a
tree includes not just the parent block, but also any "uncles";
competing prior blocks. Thus, the CPU power spent producing an "uncle"
is not wasted. It still secures the chain.

In any case: I think most of the downsides of larger block sizes also
apply to when you have shorter block times: fees go down, processing
costs go up.

## Fees are necessary?

If the block size is too big, then the cost to adding a tx is
basically zero. So fees will stay low or zero. Without any scarcity,
miners cannot recover the costs of mining from transaction fees. It's
a tragedy of the commons.

If there are zero miners, of course there is no tx processing. But
what if there are simply not that much CPU resource dedicated to
mining? After all, the difficulty is adaptive.

But I think if not a lot of CPU is going into mining, it makes it easy
for a single individual to buy enough CPU power to attack the
chain.

You might argue: if the chain is worth someone attacking, shouldn't it
be worth someone protecting? But the problem is: there's no incentive
structure.

## Computational downsides to larger block sizes

Larger block sizes means:

* More bandwidth needed to download blocks.
* More storage needed.
* More processing power to hash.

In particular: people are worried that increased costs will mean that
home users won't be able to participate in bitcoin mining any
more. And the worry presumably is: higher capital costs to mine means
more centralization.

Then again, home users already can't mine because ASIC, right?

If the costs increase for mining, can't that be recovered by tx
processing fee? But maybe not because no scarcity?

## Miners not verifying transactions?

If verifying the chain becomes expensive, miners will just trust their
pool administrator about what is the most recent block hash, and then
just search for the next nonce value.

Effectively, validation that transaction inputs are sufficient, that
signatures are correctly signed: this work is being trusted to a small
number of pool administrators.

The fear is that if this happens, what is the point of mining? We're
trusting the administrators at that point. At that point, can't they
colude, in which case they don't need to be doing all this
proof-of-work...

## Is higher TPS/lower fees necessary?

Some argue that proof-of-work will never allow high TPS regardless. So
shooting for Visa levels of tx processing is silly. They argue that
Bitcoin is more like digital gold, or possibly as a settlement layer
for transactions in other networks.

Basically: should Bitcoin be used for micropayments on the order of
one penny?

## Segwit and Transaction Malleability

"Witness" is the signing of a transaction. Segregated witness means
separating the signature from the transaction data. When computing
block size, you divide the witness data size by 4.0. That reduces the
"space" taken up by witness, which effectively increases the block
size.

Why is this better than just increasing the block size?

I think the answer is: it isn't. SegWit was meant to solve a different
problem: transaction malleability. Transaction malleability makes it
hard to implement "second layer" protocols, like Lightning Network. It
just so turned out that SegWit also increases block size, which is a
possibly more direct way it increases TPS.

**Malleability of TXID**

Malleability refers to transaction identifier malleability. The
transaction id is a hash of the inputs and outputs. It lets you see if
your transaction went through: you query the network and ask if the
txid is present in the blockchain.

Before SegWit, the txid hash also was based on the signature signing
the transaction. The problem was: there are ways to modify a signature
such that it still is a valid signing of the transaction. For
instance, the format of the signature has not always been strictly
enforced. Likewise, there can be multiple signature values that sign
the same transaction.

**Malleability: Bookkeeping Trick**

The attack was: Alice submits a transaction to the network that pays
Bob. Bob is listening to the network. He mutates the signature and
also submits; he's hoping to beat Alice's original. There can be no
double spend: either Alice or Bob's transaction is accepted, the other
rejected.

The trick is this. Bob then tells Alice: you never sent me the
bitcoin. Alice checks: she looks up her txid, and finds it
nowhere. She says: okay, I'll send again. Important: she has to spend
other inputs. Basically, she has to have a bookkeeping failure: not
keeping track that her prior transaction *did* go through.

**Invalidating chains of transactions**

Another attack has this form. The input to a Bitcoin transaction is a
prior txid and the index of the output destination. The input is *not*
simply an address. Thus, say Alice pays Eve. Alice's tx is not
confirmed yet. Still, Eve pays Bob by saying: I'll pay you with the
output of the Alice transaction. Eve submits her tx to Bob to the
network; the plan is that both will go through. But *then* Eve mutates
the Alice transaction, changing its id. Thus her promise to Bob has
been invalided; it references an id that doesn't exist.

This attack seems silly. If Alice's transfer to Eve is unconfirmed,
Bob cannot know that Eve won't submit a contradictory
transaction. Likewise, Bob should not provide Eve any service until he
sees his own transaction confirmed.

Except for off chain transactions: see Lightning Network.
