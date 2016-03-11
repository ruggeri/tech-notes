This is a summary of the [Raft Consensus Algorithm][raft-pdf]. Raft is
like Paxos, but a little easier to understand.

[raft-pdf]: https://ramcloud.stanford.edu/wiki/download/attachments/11370504/raft.pdf

## Basic Idea

The basic idea builds on top of state machines. State machines undergo
a series of transitions, each of which mutate the state in a defined
way. You can see a SQL DB as a series of transactions, each of which
update the store.

A machine can fail or otherwise become unreachable. If that happens,
the service goes down. One step toward solving this problem is to
replicate all transactions to a slave. The master can forward
transactions to the slave. When the slave confirms saving the
transaction, the DB can perform it itself. If there are two slaves,
then so long as two remain operational, the system can be recovered
manually promoting a slave to the master.

This is possible using human intervention. However, we would like to
automatically elect a new master.

Note that a human just can't supply a list of masters. Otherwise, with
a loss of connectivity, someone may *think* they've become master,
when another master is still running...

## Leader Election

The key to a consensus algorithm is leader election. In Raft, servers
can be in one of three states: follower, leader, or candidate.

When the servers start, they are all followers. Raft servers will
remain followers so long as they continue to receive heartbeats. The
timeout window is randomly sampled.

If a Raft server does not receive a heartbeat from the master within
the timeout window, it will move to candidate mode. It will send
requests to all the other servers requesting they vote that the server
become the new leader. If a majority of votes are sent back, the
server switches from candidate to leader. A candidate cannot move to
leader without a majority of servers' agreement.

Each server stores a "term number". When a server transitions from
follower to candidate, it increments its term number. It sends this
term number when requesting votes. If the prospective voter has stored
a term number greater than or equal to that it receives from the
candidate, it may ignore the candidate: it is too late to vote in that
term.

However, if the proposed term really is new to a server, it will send
the candidate its vote. The voter updates its term number. It will no
longer participate in previous terms: it will begin ignoring
heartbeats from old leaders. The voter also will not vote again this
term; a server casts at most one vote per term.

A candidate that has received votes from a majority of servers
transitions to leader and begins sending heartbeats. At most one
server can be elected leader in each term. It is possible that no
leader is elected. Candidates will time out and repeat an election for
a greater term after a random interval.

Notice that random timeouts are being used so that there aren't
pile-on effects and the algorithm converges to success.

A candidate that receives a heartbeat with a current or greater term
number will stand down.

**One question**: it appears that if any server fails to receive
heartbeats from the leader, it will trigger an election. What if the
leader cannot reach X, but both the leader and X can reach all other
servers? Then X will trigger an election even though everyone else was
happy with the old leader?

This differs from a classic partition: a set of servers gets cutoff
from a second set. Then someone in the set partitioned from the leader
will try to start an election, but it can only talk to other
disconnected servers.

Perhaps this is a weird failure mode. Everything would still work, but
performance would clearly degrade.

## Log Replication

This achieves the property that there is one leader at any given time.
Let's build a distributed log on top of this.

Clients send requests randomly to a member of the pool. If the server
is following a leader, it redirects them to make the request to the
leader.

The leader will write the entry in its log and will ask the other
servers to do so. When N/2 have (in addition to the leader), the
server may consider the entry committed and reply to the client.
Obviously, the committed entry is expected to be durable.

Because followers may not receive instructions to insert new entries
in the order that the leader commits them, the leader keeps an
incrementing entry index. Every time the leader issues a command to a
follower to append an entry, the leader will give this index. It will
also give the term number and index of the previous entry (this entry
may even be from a previous term in which the current leader used to
be a follower). The follower must not insert the new entry until it
has inserted the prior entry.

**This ensures that, when a leader commits an entry, a majority of
peers have the same entry history up to that entry.**

Note that if an instruction to insert an entry is for any reason not
acknowledged, the leader will continue to repeat it throughout its
term. Since these instructions are idempotent, there is no harm in
doing so.

Followers can go down or come up, and eventually they will come to be
up to date with the leader.

However, the leader may of course crash. In this case, a new leader
will be elected. This leader's history may differ from some of the
followers. In that case, its instruction to insert a entry will fail
the consistency check. The follower will inform the leader of the
failure, and the leader will then send a new instruction to insert the
appropriate previous record. That may fail, causing the leader to send
even earlier entries.

Eventually the leader and follower will find common-ground. The
leader's instructions are then honored, and the follower rewrites its
history.

**This ensures that the leaders view of the world will eventually win.
However, it obviously introduces the strong possibility that it will
overwrite committed entries.**

## Restoring Safety

The trick is never to elect the wrong leader. When a candidate asks
for a vote, it sends the term number and index of its most recent log
entry. The voter no longer blindly votes for this candidate. Instead,
it sees whether the candidate's log is as "up-to-date" as its own. It
only votes for the candidate if (1) the candidate's entry's latest
term number is at least equal to its own latest entry and (2) the term
index is at least as great.

This is almost enough. A candidate needs a majority of votes. This
means that amongst its voters is at least one server that has recorded
the latest committed entry. Since this server would not vote for the
candidate if it did not include this record, the candidate that wins
an election must have all the committed records.

However, we need to remember the post-hoc replication of records from
previous terms.

* Consider that Term Leader 1 adds a write to its log, but fails
  before it can ask the two followers to write it.
* Next, consider that Term Leader 2 is elected. It writes a differing
  record with term ID 2, but again crashes before it can ask the others
  to replicate.
* Presume that Term Leader 1 recovers and is elected Term Leader 3. It
  asks the original follower to replicate record 1, which the constant
  follower does. This is now committed.
* Lastly, Term Leader 1 fails and Term Leader 2 is re-elected. It can
  be elected because its latest entry is from term 2, whereas the
  constant follower's latest entry is from term 1.
* Term Leader 2/4 instructs the follower to overwrite the record from
  term 1 that was committed in term 3.

This is much better illustrated in the paper.

The solution is a little subtle. The follower should write record 1 to
its log, but it should not "commit it" (update the state). Neither
should the new leader. They should wait until an entry from the
current term is recorded by a majority of the servers. Then the old
entry can be committed. After the new term's record is recorded, it is
not possible to re-elect a leader that would overwrite the old history.

## Details

**Membership Changes**. You have to be careful as new members enter
the quorum. They won't have up to data, stuff that was previously
committed might drop below `(N+1)/2` peers.

**Snapshots**: You don't want to store the entire history: you want to
perform log compaction. Whenever an entry is committed, it is eligible
to be compacted.

However, you want to hold onto some changes in the log, since you can
send these to slow followers, since it is easier to send a diff.

For exceptionally slow followers, or new entrants, you can do
something to send them a snapshot.

Whatevers.

## Clients: Writing, Reading, and Linearizability

As mentioned, clients can randomly pick a server to communicate with.
It will direct them to the current leader.

Writes are linearized by the leader. However, there can be "submarine
writes" where the leader replicates to a majority, but dies before it
can respond. There is even the possibility of committing **after** the
leader fails (see above for an example).

Clients can be sure their instructions are idempotent by using serial
numbers for each command.

Reads don't need to write to the log, but we need to be careful.
First, the leader, upon election, must issue a no-op commit to commit
all previous transactions in its history. It then tracks the latest
commit. It must also issue a heartbeat for each read; to make sure it
is still the leader and no one else has written anything to the log.

You could also distribute reads by allowing clients to query any
follower. Obviously this could fall behind the leader, so stale reads
could result. However, if the client provides a txn id, then stale
reads can be detected and retried.

## Performance

If there is one follower, then the master must always wait for the
follower to acknowledge. One of the nodes can fail and the other can
take over with no data loss.

There is some distribution on time to acknowledge. This has probably
bad 95% latency. However, because you only need to wait for `(N+1)/2`
to ack, the 95% of the system is better. So the more nodes you add,
the higher your resiliency, but also the lower your 95% latency.

It's a win-win!

## Alternative: Fixed Master

What do distributed state machines buy us over having a fixed master
and slaves?

One setup is to have the master make a write, and forward it to the
slaves, wait for them to ack the commit, and then ack the client.
Writes have durability, but we have to wait for acks. One naive
approach is to wait for all slaves to ack, which hurts latency.

This isn't really a full answer: what if a slave fails? How does the
master deal with that? To tolerate slave failures, the master should
wait for a majority of acks, not unanimous acknowledgment. A minority
of machines can fail and the entire committed history will survive.

A slave can ack if it has processed all previous transactions plus the
current transaction. That means at least one machine is always ready
to become the master.

In a more complicated setup, the slave can ack as soon as it records
the transaction to its log. Since if a minority fail every transaction
will be recorded somewhere in the surviving set we can rebuild. This
makes failover slower, but acknowledgment faster.

Just like before, we can distribute read load if either (1) we
tolerate stale reads or (2) use txn ids.

Regardless, when the master fails, a human needs to intervene and
select a new master. This will take no less than minutes, probably
closer to an hour. The human must promote the slave which is farthest
ahead, or trigger a rebuild of the log. Master failures may be rare
but can happen at any inconvenient time, and there are many services
to monitor.

Note that the human not only needs to tell a surviving slave to act as
master, they must also tell the slaves to respect this master. If a
partition happens, when that minority of slaves comes back, they must
be told about the new master. We don't have self-healing here.

Takeaway: we can implement distributed state machines ourselves, but
Raft and Paxos automate this process and accomplish failover in
seconds.

**Another alternative**: If bounded data loss is okay, the master can
immediately ack the client, making the request that the slaves
replicate async.

## Write Distribution

We've achieved a distributed state machine system with automatic
failover. We've seen how we can use this to distribute reads by either
accepting.

However, we don't have the ability to distribute writes; they all have
to go through the master. One possibility is to partition the data and
have each partition managed by its own Raft quorum, but this prohibits
transactions spanning partitions.

Another problem is limited ability to parallelize queries. Raft is
assigning transactions an order. SQL databases typically aim to
achieve behavior consistent with some serializable ordering. By
enforcing a transaction order, Raft may be hurting DB parallelization.

## TODO

We can now operate a small, available DB. We see one way in which we
can increase read load.

How do we either (1) scale write load or (2) scale storage to greater
than one machine (which also impacts write capacity)?

A natural next step is to look at Google Spanner.
