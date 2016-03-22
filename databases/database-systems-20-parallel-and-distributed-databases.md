## Parallel Database

* Split up rows across machines.
* Can apply a projection or selection at each machine.
* To do a join, may need to do a MapReduce like operation.
    * Hash by key, send to appropriate machine. Do the join.
* They assume the shipping of data between machines is actually
  *faster* than disk IO, because of high speed network in a rack.
    * Infiniband has >97Gbit throughput??
    * SSD has ~6Gbit?? Whoa!
* When receiving data, it might be necessary to write it out to
  disk. But even then, this is all being done in paralle.
    * Also: if you can do an in memory join that won't be needed.

## Distributed Database

* Here, machines may be farther apart, and communication cost may be
  non-negliable.
* But can probably be a lot bigger. Can have higher survivability. Can
  store data closer to where it is needed.
* Mention horizontal and vertical sharding.
* Mentions that we might want to have replicas for higher read
  throughput. But then how do we keep those in sync?

## Distributed Joins

* If we want to join two relations living on two machines, we can
  send a copy of one to the other and compute the join there.
    * Send the smaller relation.
* Or, we could send a projection onto just the join keys and send
  back the relevant records from one relation.
    * Send the projection from the bigger relation, sending just
      the relevant stuff from the smaller relation.
* They do a lot of work showing how you can figure out how to do
  your semijoins in a way where you don't send any records that
  will be "dangling": i.e., don't have anyone to join with.
* To avoid joins, I could see denormalizing the DB, and using array
  columns instead of junction tables.

## Two Phase Commit

* Coordinator tells each node what to do.
* Each node can back out at any time, the coordinator will
  abort the other people.
* Once the node enters precommit, he cannot abort unless the
  coordinator tells him to.
* Once the coordinator hears back from everyone he tells people to
  complete the commit.

**Recovery**

* Say a commit site fails.
* Typically, if the coordinator doesn't hear back a `PREPARED`
  statement soon enough, it will abort the transaction.
* When the site recovers, we look at the log for a `COMMIT`,
  `ABORT`, or `DON'T COMMIT` entry. If we find it, we just do the
  appropriate thign.
* Else, if we don't find a `PREPARED` statement, we can just
  abort, telling the transaction manager. No one else could have
  committed.
* If we *do* find a `PREPARED` statement, we need to check with
  the others to see if this was in fact committed.
* NB: When a site is down, we really can't make any progress.
    * Not necessarily, see below.

**Cordinator Failure**

* We can bring a new coordinator back up, either manually, or via
  Raft.
* The coordinator can check with everyone about pending
  transactions: were they committed or aborted? It can tell people
  to finish these.
* The problem is when both a coordinator *and* a site fail:
    * In the case where all the remaining nodes had send back
      `PREPARED`, we don't know if the coordinator order us to
      `COMMIT`, and the failed site did actually commit.
    * OTOH, we don't know if the failed site told the coordinator to
      abort.
    * We have to block until at least the coordinator or the site come
      back up.
    * In this way, 2PL is *blocking*.
* NB: When a site is down, isn't the service unavailable?
    * Not necessarily *all* of the service. Maybe we can work around
      the temporary unavailability of that node.
* We say that 2PL is *blocking*.
    * If both the coordinator and a site fails, no one can commit or
      rollback until the coordinator tells us how to proceed.

## Three Phase Commit Addendum

* Here, after the coordinator gets messages from every node saying
  that they can commit, it sends a first message: "everyone agreed
  they can commit".
* After everyone acks this, the coordinator tells the nodes to
  commit.
* This helps, because, if the coordinator *and* a site node both
  fail, then everyone knows that it was *possible* for every node
  to commit. They can proceed with the commit.
* And, if not everyone has a copy of the precommit message, we
  know that *no one* could have started to commit, so we can
  rollback.
* You have to be careful of partition though. For instance, if
  someone didn't receive a prepare commit, but is partitioned
  away, they may think it is okay to abort the transaction!
* 3PL is meant for stop failures.

## Distributed Locking

* You need to be able to lock entries. In particular, there may be
  replicated data, all of which should be locked together.
* A central locking system won't do, since it becomes a point of
  contention, and also because it can fail, at which point no locks
  can be acquired until it comes back up.
* Assuming no replication, we only need to lock at the site of the
  modification.
* When you have local locks, but also replication, how can you get a
  "global" lock on all the replicas?
    * Can have a primary copy.
    * Another possibility is to say that you can write when you have
      `x>n/2` exclusive locks, and `s` local shared locks for a global
      shared lock (where `s+x>n`).
    * A typical approach: only need one shared lock, but all the
      exclusive locks.
    * You might also like to use `x=s=(n+1)/2` locks so that you can
      broadcast and mask latency.
* The whole thing seems silly since why don't you need a lock on all
  the replicas to write? Won't they get out of whack?

## Chord

* All peers are in a circle, each has links to next, and also to peers
  at exponentionally increasing distances around the circle.
* We place information `(K, V)` at the lowest node `n<h(K)`.
* Search is logarithmic, now.
* Note that to join, you hash yourself, now you know who you should be
  *before*. So it would be nice if we kept prev.
    * But note that prev isn't that vital.
    * So we're mostly inserting into a singly linked list here.
* It isn't hard to enter the circle.
    * First, you choose an ID.
    * If you know even one peer, you can find your prev/next machines.
    * You can ask the prev for the information it has that you should
      have.
    * You run queries to the circle to build your own finger table.
    * You're ready to insert! You check with your next to see if he's
      changed his prev. If so, repeat. Else, set you as prev!
    * Finish by setting yourself as next for your prev.
* When nodes leave, they should notify their prev/next.
* But if hard fails, then the nodes will eventually find out.
* Typically replicate data to avoid losing it when a peer leaves.
* I hand waived a bunch, but this book did too!
