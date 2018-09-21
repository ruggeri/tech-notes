Spanner tries to bring ACID to a distributed database. The main problem
is transaction isolation when transactions can span partitions.

## One Partition Solutions

If there is one partition, you have the two primary techniques:
two-phase locking and optimistic MVCC approaches.

You can add in replication by having a primary, which can synchronously
ship updates to the followers. If you want monotonic reads, you can use
a timestamp.

You can do async replication, but that will threaten data loss. You can
do semi-synchronous replication, but that limits the number of failures
you can tolerate.

**Paxos for Leader Election**

Since a primary can fail, you need a leader election process. That
brings in Paxos. You can use Paxos to elect the leader. The Paxos group
can hand out time-based leases. I think time-based leases intrinsically
need drift assumptions, so that if a leader is partitioned off, it will
realize it is no longer leader and must stop accepting writes.

In this setup, Paxos is *only* used for leader election.

If you want to do synchronous writes (a goal of Spanner), how many
machines must you replicate to? You must replicate to more than half,
since you must be sure that a Paxos quorum for a new leader is
guaranteed to have the update.

**Paxos as Distributed State Machine**

An alternative is to run Paxos as a distributed state machine. All
writes can go through Paxos. You could put SQL statements through Paxos,
to specify a serial order. But they would have to be executed serially;
they could not be concurrently processed as this would make the SQL
statements have non-deterministic effect.

A better approach might be if one leader executes the SQL statements,
and writes the logical record updates into the log. Thus your Paxos
group becomes a replicated log.

At this point, your primary/backup solution has been folded into Paxos.
An advantage is that you get the guarantees of Paxos. When you try to do
log shipping outside Paxos, I think you just end up doing an equivalent
amount of work (unless asynchronous or semi-synchronous is okay with
you).

**Multi-Paxos**

Multi-Paxos is just Paxos optimized for long leaders writing many
updates to the log. It has less message overhead. I think it's a perfect
fit here.

*TODO*: READ MORE ABOUT MULTI PAXOS.

**Reading Values**

With a general replicated state machine, you need to send read messages
to the Paxos group. This is necessary for serializable reads. Otherwise
you don't have read-after-write even.

If you want to do reads at a timestamp, you can talk to anyone. They may
not be able to answer your query if they don't have an up-to-date
version of the value. But most of the time this should work fine.

## Multiple Partitions

You can't handle the storage or the write load, so you want to scale out
by adding partitions. If you don't want cross-partition transactions,
then you are done.

You can't just run Paxos with a huge group; that wouldn't reduce write
volume per node at all. Thus, you have a complicated problem if you want
to maintain ACID semantics.

**Two Phase Locking With 2PC**

One approach is to use two-phase commit and locking. Your transaction
takes out locks on the data it uses. When it has all the locks it needs,
and the machines all acknowledge as prepared, the coordinator can tell
the machines to commit.

If your coordinator dies, you can't release the locks. Well, any machine
that hasn't prepared can release its locks. Any other machine has to
assume that maybe the coordinator did tell others to commit.

Your other major problem is that writers block readers, and vice versa.
If a transaction involves a number of partitions, the latency can be
high. There can be a feedback loop effect: transactions take longer, so
its longer to acquire locks held by other txs, so txs take even longer.

2PC can fail. But they run 2PC with Paxos, so that there are replicas
that can recover and take over.

**Problems With Distributed MVCC**

To stop blocking, can we use (locking) MVCC? It's not so easy, because
there is no global clock.

A way MVCC can work on one machine:

1. Readers run with timestamp of earliest pending write.
2. Writers take locks. When ready to commit, they enter the "pending"
   mode. This is the moment you decide the timestamp. You then go in and
   write all the timestamps in.
3. When the writing of all timestamps is done, you can remove the
   transaction from the pending set.
4. One key point: you don't remove someone from the pending set until
   all prior txs have been fully flushed.

**How Cockroach Does It?**

If you want to do MVCC in the distributed case, you have the problem
that you don't have a single clock. When you do a read transaction, you
must pick a read time *up-front*, so that you can read this snapshot.

If you start reading records, you must wait until all participants agree
(1) they have received all writes from at least that time, (2) no
pending transactions will be given an earlier timestamp.

If you are reading and writing, you again choose a time up front to read
at. As you read and write you perform locking. At the end, you have
everyone prepare for commit. They each pick a timestamp; the issued
timestamp will be the greatest of any of these.

Once the machine gets the commit command with the final timestamp, it
may commit with that timestamp.

**Any Problems?**

So far, so good. The CockroachDB people explain that the scheme
described offers serializability. But it doesn't offer linearizability.
That's because I can:

1. Modify a record at partion 123 with a leader with a fast clock at
   100ms. Wait for this to commit.
2. Next, start a transaction to create a record at partion 456 with a
   slow clock of 50ms.

Now, if I historically look at things, it looks like the document was
created before the user. I've violated linearizability (serializability
is okay!).

How can this be problematic? Say I want to do a *read* across
partition 456 and 123. If I start the read at the machine with slow
clock and choose a timestamp of 75ms, I'll read the second record but
not the first. This is a failure of linearizability. Note this can
only happen if there is skew.

I can fix this if I use a "causality token:" which is just the timestamp
that tx1 commited at. This is fine if I pass it around. But what about
out-of-band processes? Cockroach people seem to think this is unlikely
to be problematic.

**Reading Most Recent Data**

Let's say I want to read the current data and display it. I want to
choose a point in time and want to see all data that was written at that
point in time, and none that came after.

Anytime after the start of the request is fine.

So I start with the current time when the read transaction is issued. If
it is too far ahead for some node, I may have to wait for that node to
go past that time. (Also matters what pending transactions are open).

The other problem: what if it is too small? Consider this:

1. Data is written to node1 at 100ms. Finish.
2. We start reading, but we use a timestamp of 50ms.
3. We don't see that written data.

Basically, the Cockroach people don't want to accept write-read
linearizability violations. But how?

There answer is: when the tx starts, Cockroach will also note what is
the latest possible time that ANY server thinks the present could be.
That is, it selects: `(read start time, read start time + max clock
ahead)`.

Cockroach *could* wait for all that time to pass. But that would be
unacceptably slow. **That's what Spanner does.** But because of the GPS
clocks it's only 7ms. For Cockroach it is more like 100ms.

Cockroach won't wait. It will start trying to read. But if it encounters
any data that *could* be in that danger window (i.e., could have been
written before the read request ever started), Cockroach will try to
start again, but with that later timestamp.

It will keep the same upper bound though!

Source: https://www.cockroachlabs.com/blog/living-without-atomic-clocks/

**More Specific About How Spanner Uses TrueTime**

When reading, it sets the latest possible time as the time stamp. That's
what the Cockroach people said it did. It waits until that time has
passed. That ensures write-read linearizability.

When writing, we also wait for the chosen timestamp to finish. This is
important for the write-write linearizability we talked about above.

They talk about how to mitigate read delays. If a read hits just one
partition, you can wait for the time of the most recent tx to pass. That
ensures the read data was assigned a timestamp truly in the past when
you return the data to the user.

When reading from multiple partitions, you *could* do an extra round of
communication. You'd take the max of the timestamps read, and wait for
all these to pass. Note: you want to choose the minimum safe timestamp,
but that means the maximum

They talk about how the timestamps can be granular. Duh; I wouldn't have
wanted to do this for the entire node!

## Takeaways

* They use Paxos as the means to coordinate synchronous replicas.
* They do distributed transactions with locking and 2PC.
  * I think that the Cockroach way with the switches prolly allows write
    skew. But they have an SSI mode: this uses transaction restarts I
    think.
  * https://www.cockroachlabs.com/docs/stable/transactions.html#isolation-levels
* Reads don't block writes. Writes don't block reads until the prepare
  statement.
  * Reads can be long running. Writes can also be slow if conflicts are
    rare.
* Offers linearizability, not just serializability.

## Questions

* Is the whole point reads don't block writes vice/versa?

https://jepsen.io/consistency
