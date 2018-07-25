## Lightning Network

The idea: Bitcoin can be slow because everyone must know about every
transaction. The Lightning paper notes: if every tx is on the
blockchain, and you do 47k TPS, then that's 8GB of data every
10min. They also want to even have higher TPS, in order to enable
micropayments.

Consider two parties that do a lot of business with each other. What
if they do things "off chain"? They make transactions to each other,
but just don't publish these. Since these transactions are made
point-to-point, there is no need to tell the rest of the world. This
can be fast.

When the parties are done and want to settle up, they submit the
entire log to the blockchain, which processes it in one go.

The problem, of course, is that one party spends the coins on which
their payments on the side chain are based. Now that those coins are
gone, the side chain payments are invalid.

To ensure that doesn't happen, I want my counterparty to publish to
the block chain: we can only spend these coins if we both agree. So we
pay back and forth, and when we are done paying each other, we can
both agree to close out.

But then a problem is this: Alice says she won't spend coins without
Eve's permission. Alice doesn't run up any charge with Eve afterall,
and eventually wants her coins back. But Eve refuses to countersign
the closing of the locked account. Instead, Eve blackmails Alice.

So we have two problems:

* Eve doesn't want Alice to spend the money without her permission.
* Alice doesn't want to give Eve total refusal rights to spending the
  money.

The solution is realizing what Eve really wants. It's okay for Alice
to close and spend the locked money, so long as Eve still has a window
of time to demonstrate that Alice in fact spent the money to her off
chain.

The way this presumably happens is they use a clock. For instance,
they might say: Alice can't spend the money unilaterally for 100
blocks. Eve makes sure to present all transactions from Alice before
block 90. At block 90, Eve, should accept no more payments; another
channel will need to be set up.

Indeed, some timing feature like this needed to be added for LN
(Lightning Network).

**Must not have malleability**

Transaction malleability is a problem here. Since these transactions
are happening off chain, you have to trust that when Alice spends out
of tx1 with txid1, that she won't mutate tx1 to have txid2. That would
invalidate her payment to Bob.

**Beyond Bidirectional Channels**

I want to be able to send money from A to Z, by going through a number
of intermeidate channels. But how does A send money to B, trusting
that B will send it onward to Z.

I think the idea is to have smart contracts. As in: this transaction
only goes through if you demonstrate that are you part of a chain that
ends with Z.

So let's say Y wants to close out with Z. Y has a promise from X to
pay. Y can close out with Z. Y knows that they can prove to X that
they did as they said and get the money from the funded channel
between X and Y. Now, since the contract between X and Y is time
limited, Y may ask X to sign an unlimited contract now that Y has the
proof. X should kindly oblige. But if X should refuse, Y can always go
to the network and force settlement.

## Downsides

* LN may reduce need to settle on block chain thus reducing mining
  fees. Which as discussed can be a bad thing (less incentive to
  secure the network).
* People have to constantly watch the block chain to make sure their
  counterparty doesn't try to close out the channel without them. But
  that honestly doesn't feel problematic.
* To efficiently route, nodes in the Lightning network would need a
  lot of open capital accounts. Basically, they would be banks. The
  problem with the bank is that it is centralized.
    * Is this really a big deal? We're talking about a bank where no
      one can stop you from removing your funds or stealing them.
    * There's nothing stoping people around the world from
      incorporating their own banks. So it's hard to see how cartels
      would form.
* If you're operating as a broker, you're probably subject to bank
  regulation. But that's the whole game of bitcoin: it's beyond the
  reach of nations.
    * Or is it? People need to transaction in Bitcoin. Which means
      they are only good if you can buy shit with them.
    * And states can put the hurt on real-world businesses.

Paper: https://lightning.network/lightning-network-paper.pdf
