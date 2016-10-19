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
  disk. But even then, this is all being done in parallel.
    * Also: if you can do an in memory join that won't be needed.
* You may have more total IO required on a parallel system, but this
  is done in parallel, so throughput can be higher.
    * You can also get super-linear scaleup if the amount of work done
      at each partition allows for an in-memory algorithm rather than
      an external algorithm.

## Distributed Database

* Here, machines may be farther apart, and communication cost may be
  non-negliable.
    * This is what distinguishes a shared-nothing parallel DB and a
      distributed DB.
    * It's just an assumption on the geographic location of the
      machines, and the communication cost.
    * A few thoughts:
        * Bandwidth may be very expensive between datacenters.
        * Latency may be high. E.g., 75ms from NY to SF.
        * Bandwidth between datacenters may be ~40Gb over fiber.
        * But you can always pay for more. But it sounds very
          expensive.
* But can probably be a lot bigger. Can have higher survivability. Can
  store data closer to where it is needed.
* Mention horizontal and vertical sharding.
* Mentions that we might want to have replicas for higher read
  throughput. But then how do we keep those in sync?

## Parallel vs Distributed

* Parallel DB can have high performance and support all normal SQL
  operations.
    * Actually I'm kinda skeptical that your bandwidth can increase
      linearly like your machines.
    * I guess if you have point-to-point connections, but that's
      `n**2`.
* Distributed DB is mostly about addressing latency problems when
  reading sites are distributed over the planet, and about ability to
  not make the service unavailable to Dallas customers when Detroit is
  down.

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
    * In this way, 2PC is *blocking*.
* NB: When a site is down, isn't the service unavailable?
    * Not necessarily *all* of the service. Maybe we can work around
      the temporary unavailability of that node.
* We say that 2PC is *blocking*.
    * If both the coordinator and a site fails, no one can commit or
      rollback until the coordinator tells us how to proceed.
* When people say a failure of just the TM causes the algorithm to
  block, they implicitly assume that the TM is also a commit site,
  which has a vote, or that the TM can decide to abort for some
  alternative reason.
    * So don't be confused if they say failure of TM causes blocking,
      or need failure of TM plus a site.

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
    * This seems silly. It says 3PC only works for no partition
      scenarios.
    * But partitions can happen. So what use is 3PC?
* 3PC is meant for stop failures.
* NB: I think that 3PC is a better version of "replication", where the
  TM has a slave replica. The only thing important that the slave
  could know is whether the TM decided to commit. And 3PC has the TM
  send that.

## Distributed Locking

* You need to be able to lock entries. In particular, there may be
  replicated data, all of which should be locked together.
* A central locking system won't do, since it becomes a point of
  contention, and also because it can fail, at which point no locks
  can be acquired until it comes back up.
    * Also contention.
* Assuming no replication, we only need to lock at the site of the
  modification.
    * But you need to have a *coordinator* lock each of these first,
      and wait for a reply. Last, it must release the lock.
    * So there are 3 messages per lock site.
* When you have local locks, but also replication, how can you get a
  "global" lock on all the replicas?
    * Can have a primary copy.
    * This can be efficient if the transaction doing the locking is
      most likely to be at the site of the primary copy.
    * E.g., if you have an inventory database and many stores, you
      could put the primary copy of items at the store where they are
      at.
    * You have replication for durability, but writes are likely to
      come from the store the product is at.
    * Failover for primary copy probably involves taking a lock from
      Chubby, which will ensure only one person gets it.
    * Old primary can learn they are no longer primary because they
      don't have the lock anymore.
    * Presumably primary must keep renewing a global Chubby entry.
        * Indeed, we must be careful about clock sync.
        * The Chubby paper says that we must be able to make
          conservative assumptions about differences in clock speeds!
        * I AM NOT CRAZY! Yes!
* No Primary
    * Require `x` local exclusive locks for a global exclusive lock.
    * Require `s` local exclusive locks for a global shared lock.
    * Need `s+x>n`: so that if you have a shared lock, you don't allow
      an exclusive lock (and vice versa).
    * Need `2x>n` so that you can't have two exclusive locks
      simultaneously.
    * One way to do this is set `s=x=(n+1)/2`.
    * Alternatively, if you want to optimize for read throughput, you
      might set `s=1` and `x=n`.
* May want to set `s=x=(n+1)/2` to ensure that if there is a
  partition, at least one side will be able to process transactions.
* I am unclear why you don't need to lock all sites for a write?
  Everyone needs all the writes eventually. Maybe at the time of a
  write, the nodes involved should sync their history? Because it
  sounds like otherwise exactly one node might have a linear history,
  but the others will not contain the full record of all the
  transactions...
    * Maybe you require that a read combine information from all
      locked read sites.
    * Note that *no* replica is guaranteed to have the entire
      transaction history! But any majority will!
    * One way to do this: keep version numbers with a record. Always
      read the highest version numbered copy locked. When writing,
      always write with a higher version number than the current
      highest.
* Primary Copy
    * It's just like leader-follower replication.
    * You could do it sync, I guess.
    * Or it could be done async. In that case, maybe some replicas
      will be a little stale.
* It feels like these locks should not be allowed to
  timeout. Otherwise, a change may be accepted at some sites but not
  all.

## 2PC vs Distributed Locking

* 2PC is just about commit/abort. It's not about locking.
* Say the operation was "append this line". Say we want to make sure
  the line is appended both places.
* We can use 2PC to ensure that the line is either appended to both
  files, or none.
* We do not need to exclusively lock the file to do this, if ordering
  does not matter.
    * But then I'm not sure what we're really commiting to.
    * Maybe there's limited file space, so the site reserves some
      space.
* But if we do want to ensure the files have lines appended in the
  same order, we do need to do locking.
* The resources to be locked may be replicated. They may especially be
  replicated across datacenters.
    * Therefore, maybe a read lock on just one version is fine, but
      need all versions for writing.
    * That would bias us toward good throughput on reads.
    * It would make it hard to write, if any copy is down.
* If we want true ACID, we should not release *any* locks until we are
  confirmed that the write went at all logical sites.

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
