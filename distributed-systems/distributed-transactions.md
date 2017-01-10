# Motivation

* You want to scale writes to a service.
* So you need to shard.
* But now you need cross-shard transactions.

# Two Phase Commit, Paxos

* If the coordinator and a commit site both fail, then you can't
  unlock the other rows at other shards.
    * In particular, any commit sites partitioned away just hang until
      the partition heals.
* That means they are unavailable.
* Plus, if the shards live in different datacenters, latency is super
  high. Since the transaction requires two round-trips between
  datacenters, we're talking hundreds of milliseconds.
* So we want to beat 2PC.
* 3PC is bogus. It can't help in the case partitions are possible.
* Paxos involves more messages, but can tolerate more failures.
* It still is high latency, though.
* Note that latency means that locks transactions can take hundreds of
  milliseconds even when locks are *uncontended*.
    * Contention means that transactions take even longer.
    * And that means locks are held even longer.
    * This is a cycle that feeds on itself.

# Life Without Distributed Transactions

* Can we deal?
* First, we need to know that action will *eventually* be taken, even
  if not immediately.
* That can be accomplished if (1) we have a durable queue and (2)
  operation is structured to be idempotent.
* I'm not evening going to worry about commutativity of transactions.
* An example is a counter that is shared at two sites. Say I receive a
  request to increment it.
    * Because of the possibility of submarine updates, I need to
      structure my updates idempotently.
    * That means the sites need to keep a log of tokens of updates.

TODO: It looks like I've never committed these? I don't know that
these are any good.

## Sources

* http://the-paper-trail.org/blog/consensus-protocols-two-phase-commit/
* http://the-paper-trail.org/blog/consensus-protocols-three-phase-commit/
