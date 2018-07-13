## Ch1: Reliability/Scalability/Maintainability

Data systems include:

0. Databases
1. Caches
2. Search indexes
3. Stream and batch processing

Qualities:

1. Reliability
2. Scalability
3. Maintainability

Reliability means fault tolerance. Fault tolerance may come from
physical equipment failure, or software failure, or human
misconfiguration.

Scalability: can be about throughput or latency. Scaling up/out. Elastic
scaling.

# Ch2: Data Models/Query Languages

Relational model: tuples, associated data is referenced by id. Document
model: you nest associated data.

Many-to-one relationships are easy for relational, but annoying for
document model because join support is typically weak. If there is no
built-in support for joins you have to write this logic in the
application (ugh). Or you can denormalize, but then you have the
responsibility of keeping things consistent.

Historically, document model was called "hierarchical".

One advantage of document DBs is that they typically enforce no schema,
which can be a pro in the sense that it allows fast evolution. This
"schema-on-read" approach is kind of like dynamic typing.

Document DBs can potentially offer better data locality, because you can
denormalize and then you don't need to chase foreign keys.

They talk about declarative and imperative querying. SQL is declarative
of course. Map/reduce is imperative(ish).

Graph databases have vertices and edges. When are they good? I don't
know. They support declarative query languages like Cypher.

They discuss triple-stores (another kind of graph db), and a couple
other query languages. Datalog was an early kind of query language. It
was like Prolog.

Other DB ideas:

1. DBs oriented around finding similarities, for instance in genomic
   information.
2. Full-text search.

## Ch3: Storage and Retrieval

Simplest DB is just a log of updates. But slow to do any kind of query.

You may want to compact logs from time to time, and keep a key-value
format. Often easier to do logging and compaction, because this makes
everything serial IO. Also, concurrency is easier.

They go into SSTables and LSM-Trees. SSTables allow (1) fast merging,
(2) fast search with sparse index. But then how do we keep things sorted
in the presence of writes?

We keep a memtable. When memtable is full we flush. We also keep an
*unsorted* version of the memtable for crash recovery. To query we just
work our way backward from memtable to older-and-older SStables. We need
to write tombstones. We need bloom filters to know what SStables to
read.

B-Trees are your other approach. Most B-Trees need only be 3-4 levels
deep, and prolly first two are in memory. You need to make them
resilient though, which is the purpose of the WAL. You also have to
worry about concurrent access, so you have to put locks in your tree
nodes.

LSM-Trees have less random IO, which is especially useful on rotational
drives. They also have better compression potential, so less storage
footprint. On the other hand, B-trees can have a better transactional
concurrency story, because you can attach range locks directly on the
tree.

They talk about clustered and non-clustered indexes.

There are also specialized indexes: multi-dimensional indexes, for
instance. They also talk about postings lists and also that Lucene uses
a trie for keys, which allows construction of a **Levenshtein
automaton**. The automaton can find similary keys by string edit
distance. **TODO**: I'm curious to learn more about this!

VoltDB and MemSQL are memory-only DBs. They do use persistent storage
for durability. Aparently the advantage derives from not encoding into
disk-friendly structures; even disk-based DBs keep things in memory.

OLTP vs OLAP. OLAP queries are normally performed on a *data warehouse*.
It is normally built async in the background, while OLTP supports client
interaction. Teradata and Vertica are some of the leaders. But now there
is Hive and Spark SQL, Impala, Drill.

Data warehouses often use *star schemas* AKA *dimensional modelling.*
The idea is that you have a table of events (AKA *facts*). For instance,
you may have sale facts. If a customer bought an item, then you have
entries for the customer and the item in their own tables.

In fact tables, you may have very wide rows. This makes column-oriented
storage and compression attractive.

If you want to write to column-oriented storage you often use LSM trees.
B-trees don't really work in this context.

## Ch4: Encoding and Evolution

JSON, XML. Also binary encodings like MessagePack, Thrift,
ProtocolBuffers, Avro.

I read this previously, but don't take detailed notes here because I
find this subject boring. Mostly it's about how to deal with schema
evolution, backward/forward compatability.

They talk a little about REST and SOAP. They note that SOAP services are
often described by a WSDL file that enables code generation.

They talk about distributed actor frameworks, where queues are used as
the intercommunication medium.

## Ch5: Replication

You do this to reduce latency via geolocation, increase availability if
some machines fail, scale read load.

Leader-follower replication. One leader accepts writes, followers can
serve reads. The question becomes sync or async. But sync becomes
unavailability if any one node does. So they have *semi-sync*, which
says that you only must write to one other node. If that becomes
unresponsive, then you choose someone else to be your sync follower.

Failover can be hard:

1. You need to be sure to rollback and discard those changes from a
   leader that weren't propagated when the leader returns.
2. You need to make sure that no two nodes think they are leader.

Replication can be SQL statement-based, but then you have to worry about
nondetermistic functions like `NOW`. You can ship WAL, but this is very
tied to the storage format. A middleground is *logical log*. This is a
textual description of what changes were made to what rows.

With async (or semi-sync) replication, you can have followers fall
behind. You have *eventual consistency*, but queries to the service
don't necessarily see results in a consistent time ordering.

You can achieve *read-after-write* consistency (read your own writes) if
you just talk to leader. But you can also use timestamp: keep timestamp
of most recent write.

*Monotonic reads* means you can't "go back in time". This happens when
you read from different replicas. Another problem is *consistent prefix
reads*. This means:

1. If a transaction T1 writes partition A, then a user reads partition A
   and takes action to write to partition B.
2. Then, a reader could see the write to partition B without seeing the
   write to partition A. This can happen if they read a follower in
   partition A with very slow replication, but then read from a leader
   or follower with fast replication in partition B.

There is also *multi-leader replication*. This normally doesn't happen
in one datacenter, but does occur in multiple datacenters. This allows
better performance and reliability, but greatly increased complexity,
and potential write conflicts.

Any client with offline operation is basically multi-leader.

You can try to avoid conflicts by having a single leader for each
record. This can work well if only one user has write access to the
record.

To resolve, you can do last-writer wins. Or when conflicts occur,
present the conflict to the user on read time to present resolution.
CRDTs and operational-transforms try to deal with.

You can also have leaderless replication. Client sends writes to all
replicas. If `n/2+1` accept, then when reading `n/2+1` you can get the
most up-to-date value.

Even if you choose `w+r> num nodes`, you can have conflicts. What about
concurrent writes? Who wins? If you choose based on LWW, you can lose
writes.

They mention *sloppy quorums*. Here, in the presence of partition, you
still let writes happen, but you save to `w` nodes in the partition you
have access to. On heal of the partition, you sync everyone. The
metaphor is: "if you're locked out of your house, you might stay on your
neighbor's couch." The sloppy quorum no longer means you will read the
latest data: you can't from the other side of a network partition. It
increases *durability*.

Let's talk about handling concurrent writes. Let's just have a single
node. For every key, you can have multiple versions, each with an
incrementing version number.

Whenever a client wants to write, you force a read first. You give the
latest version, plus all history preceeding that version. The client
then can examine the history and then write back a value. All prior
values can be garbage collected at that point.

In this single node case, why even keep versions? If your client writes
with a stale prior version, then you can just reject and say: you should
read the latest version and try again. But we need to talk about
*multiple leaders, or no leader*.

When you have multiple nodes, you need a *version vector*. In that case,
you garbage collect only when you have a version that dominates.

**Summary**:

1. Leader-follower replication has failover issue, possible
   non-monoticity of reads. Can be fixed via version numbers.
2. Multi-leader replication introduces possibility of conflicting
   concurrent updates.
3. Same with leaderless quorum updates.
4. To surface these problems, you can use version vectors, which record
   the version history.

## Ch6: Partitioning

Partitioning can be combined with replication of each partition. You can
partition by key-range. You can partition by hash.

What about secondary indexes? You can have a secondary index local to
each partition, which can be helpful in indexing the keys on that
partition, but isn't *global*. You'd have to query all of these to find
all the rows matching a given secondary key.

Alternatively, you can have a global index that's partitioned separately
from the primary key partitioning. That means more expensive writes
though: you have to hit many secondary indexes. For that reason you
typically have async secondary index updates.

Rebalancing is a problem. One approach is have a huge number of
partitions originally, store many at each machine, and when someone is
added just move a few from each machine.

If you allow dynamic repartitioning, who keeps track of what partitions
are on what nodes? Hadoop had a single leader for this, but then adopted
ZooKeeper for consensus and failover.

## Ch7: Transactions

They talk about multi-object vs single-object transactions.
Single-object transactions often support certain atomic operations like
increment, or test-and-set.

Multi-object transactions are necessary for multiple object updates, but
also just to keep secondary indexes in sync with each other.

They go through isolation levels. They give a good example of write
skew for a room booking system.

1. Two clients both verify a room is free.
2. They both write records to book it.

So how to achieve serializability? Could do serial execution. This is
actually viable when entire working set fits in memory. The problem is
really just loading from disk. VoltDB does this. You can partition to
scale write load, but cross-partition work must be done in lockstep and
kills performance and isn't improved by more machines.

Two phase locking is the other main approach. This involves lock
overhead, but they say the main performance loss is because of lost
concurrency when you could be simultaneously executing.

Another approach is SSI (serializable snapshot isolation). This is
optimistic. FoundationDB does distributed SSI (I think so does
Cockroach). Works well when there isn't a lot of conflicting updates.

I skipped over a lot because it is mostly regarding DB isolation levels,
which I have covered elsewhere in the past.

## Ch8: Trouble with Distributed Systems

Network faults are pretty common: single machines and often whole racks
are disconnected. Redundant switches don't help that much because of
human misconfiguration, a frequent cause of failure.

Network failure can be silent. You don't know whether your message was
processed. Typical to use timeouts to decide when to move on, but how
long should your timeout be? There is no timeout that guarantees that
the other party is truly dead.

Short timeouts yield more responsiveness, but more probability of
declaring a slow node dead.

They talk about difference from TCP and circuits. Circuits reserve a
certain bandwidth for the client, but it is wasted if not used. Circuits
are more effective if you know bandwidth needs.

They talk about clocks: time of day clocks can go backward when synced
via NTP. Also leap seconds. There are also monotonic clocks; NTP may
slow or speed up their rate to get to the right time.

You might try to use timestamps to order events across nodes. This can
cause loss of writes. For instance, say a replica has a slow clock. Any
writes sent to that replica will look earlier than the current state of
the other replicas, and thus will be dropped by LWW. This is what you
need version vectors for.

They talk about snapshot isolation in Spanner. First, they use the
TrueTime API: this is a GPS set clock with a confidence interval on the
time. Spanner uses this timestamp for snapshot isolation: if you try to
run a transaction at time T, and it needs data from node X, you wait
until the earliest possible time has advanced past time T.

They talk about using clocks to measure lease times. They note that a
program can be interrupted at any time. So if a leader checks that it
still has a lease to be the leader, it might timeout in between the
check and when it performs the write. A fix is a *fencing token*. The
fencing token is handed out by the lock service. The resource (for
instance, the file of a node) can reject any writes as outdated if a
newer token has already been used to write to the resource.

We assume no Byzantine faults. But you should protect against:

1. Malicious inputs from end-users,
2. Packet corruption via checksums,

They mention synchronous model (bounded delay, unrealistic), partially
synchronous (synchronous most of the time; realistic), or asynchronous
(no clocks at all; too restrictive to be truly realistic).

Also have crash-stop and crash-recovery models. The second is harder.

## Ch9: Consistency and Consensus

Eventual consistency could be called "convergence."

Linearizable means that the system acts as if there is only one copy of
the data. They give an example of simple replication: I write X2, Alice
reads it, tells bob about X2, but when he reads from a slower replica he
sees X1.

If write W1 finishes before read R1 starts, then the write value must be
seeable. If write W2 is in progress when R1 starts, we either may or may
not see W2 in R1.

However, let's say R1 sees W2. R1 may end before W2. If R2 then starts
after R1 ends (but before W2 finishes), we must also have R2 sees W2.

Basically, it should be that every operation is as if it happened at a
single moment in time during the transaction period.

Linearizable and serializable mean different things:

1. Linearizable is about one-record items (AKA registers). It says that
   each write must be as if it happened at one moment in time, and that
   this respects the ordering of the transactions.
2. Serializable is about transactions across records. It says that
   transactions must be run as if in a serial order, but this can be
   incompatible with the true transaction submission order!
3. Systems can have both: this is called strict serializability AKA
   strong one-copy serializability.
  * 2PL gives strict serializability. But not normally SSI.

When do you need linearizability? You need it for locking and leader
election, since the whole point is that all nodes agree who owns the
lock. Likewise constraints need linearizability. BTW, you want a
linearizable CAS operation to get these services.

They give another example where linearizability is useful. You want to
write an image to a distributed FS, and you want to add a msg to a queue
to read this image and resize. So you write to the distributed FS, wait
for ack, then you put the msg in the queue. Fine, right?

But what if the resizer takes image off the queue but can't see the
image in the FS? This can happen if a replica of the FS isn't up-to-date
yet.

Single-leader can be linearizable. You can then go to the single leader.
But if you do async replication you can lose writes, which fails both
durability and serializability. Even if syncronous replication, you need
to be careful that a leader "stands down" before any new leader takes
over.

To offer proper failover, you probably need a *consensus algorithm*,
which itself provides linearizability.

Multi-leader is not linearizable. Quorums aren't really linearizable. If
you use LWW, then the problem is that the timestamps can get out of sync
with real time. And they show a subtle failure mode:

1. Client A writes X2 to Peer1. Writing continues in progress.
2. Client B reads X2 from Peer1 and Peer2.
3. Client C reads X1 from Peer2 and Peer3.
4. Client A finishes write to Peer2 and Peer3.

You can make Dynamo provide linearizability if you make the reader do
sync read-repair.

They show that the CAP theorem prohibits linearizable operation in the
face of network partition.

But even the RAM of a multicore machine isn't linearizable. You need to
issue a memory barrier if you want linearizability. The reason is
performance. This is a primary reason systems don't offer
linearizability: it is too slow.

They talk about causal consistency. Causal consistency is a preorder: it
says that the store must act like anything that "happens after" must be
performed as if it happened after. Linearizability implies causal
consistency: it says there is a total order compatible with time.

When are causality and linearizability different? See the example above.
Since Client B's and Client C's reads are both concurrent to Client A's
write, and also concurrent to each other, this ordering is fine.

Why is linearizability desirable? Because you can't lose the property
because of an outside channel. On the other hand, if Client B and Client
C *can* communicate through some third channel, then causal consistency
will no longer hold: your datastore system is unaware of the other
channel.

**TODO**: they say that the CAP theorem doesn't apply to causal
consistency, and that causal consistency can be more scalable than
linearizability. I need to learn more about that.

Here's how causal consistency works (I think). You have a version number
for every record you read/wrote. When you submit a write, you send the
versions. The node waits until it has processed all those prior
versions. Then it can do what you ask.

Note that this can handle cross-object causality. We need to be a little
careful if we want handle conflicts where two concurrent transactions
try to write the same value. Note that even though causality can come
from reads of various records, we are *not* talking about transactions
here. We are still just talking about sequences of writes to a single
register.

Instead of keeping these huge vector clocks, you might want a sequence
number: a monotonically increasing clock. This is basically what is
given by leader replication. Or you can use timestamp ordering if the
timestamp resolution/accuracy is high enough.

A working way is to use *Lamport timestamps*. Here every node keeps
track of the number of ops it has performed. It increments for every
operation. The client sends the Lamport timestamp, and the node updates
to the max of the sent and current timestamp before incrementing.

Lamport timestamps are simpler than vector clocks. They imply a total
ordering that is consistent with causality. But they lose information
about what operations were concurrent. The advantage of Lamport is that
it is more compact.

What about conflicting concurrent operations? For instance, what about
claiming a username? Using Lamport timestamps you can accept the first
write and fail the second. But that doesn't help: if you promised
two users a username you already fucked up.

This brings us to *total order broadcast*. Here, we need to know when
all operations to a record have finished being applied. Total order
broadcast is a message protocol where:

1. All messages are delivered in the same order.
2. No message is ever lost: if delivered to one, it is delivered to all.

Using total order broadcast, you can do state machine replication.

The key difference between total order broadcast and lamport timestamps
is that when a message is delivered, all prior messages have already
been delivered. Lamport timestamps allow delivery of msg M1 *after* the
delivery of msg M2. Total order forbids that.

Is total order broadcast equal to linearizability? It turns out you can
write read and write linearizable registers with TOB in a crash-stop
model. This cannot be done for TOB, since it is equivalent to consensus.
So linearizable is "easier." However, to do linearizable CAS this is
equivalent to consensus. Since CAS is pretty necessary to be
interesting, the problems are equally hard.

You can implement linearizable CAS via TOB. When a message comes to
write, you send a message to write the key. The message sends the
current value of that key. You then wait for the message to be
delivered. If no one else wrote in the meantime, you do the write. Then
you can ACK.

What about linearizable reads? You can do similar: you send a read
message. When it is delivered, you read the current value.

How about the other way? You could implement TOB on top of a
linearizable CAS. You just need an integer register which is the
sequence number. When you are asked to perform an op, you do a CAS to
reserve a sequence number. You wait until all prior messages are
delivered. Then you send everyone your operation.

As discussed, linearizable CAS is the same as consensus.

**Summary thus far**

* Linearizable says that for a single, replicated record, you act as if
  there were one copy.
* Quorums are not linearizable.
* Linearizability is expensive; what if we only wanted *causal
  consistency*? For all data records you read previously, you keep track
  of their version number. Now operations need to use version vectors.
* An alternative is to use Lamport timestamps. But this loses
  information about concurrent operations. Now you can only do LWW; you
  can't repair any fancier way.
* You also can't do uniqueness constraints with Lamport timestamps.
* A solution is total-order broadcast TOB. Then you get a sequence
  number. You can use this to basically do state-machine replication.
  This can be used to implement linearizability.
* Likewise, linearizable CAS can trivially be used to implement TOB.
* But both TOB and linearizable CAS are equivalent to consensus.

**2PC**

Onward!

Consensus can be used to do leader election. Or it can be used to do
atomic commit.

They talk two phase commit. We know the subtleties:

1. Coordinator needs to write decision to commit or abort in stable
   storage before sending second phase messages. Likewise participants
   need to record if they promise to be able to commit.
2. If the coordinator goes down, the system hangs until it comes back.
3. You can hack on functionality to query someone else in the
   transaction, but this isn't traditionally part of the protocol. And
   you'll still fail if partitioned from coordinator plus one node.

2PC can have bad performance. And it can make large parts of your
application unavailable transitively.

(Note: FLP proves that consensus is impossible in an async model with one
stop failure. *But*, if we have clocks and timeouts, we can get
consensus back.)

**Consensus**

If you have a single leader, then you have consensus, except
no availability.

You can use 2PC to implement leader election, but it won't have
availability.

Consensus is typically implemented via total order broadcast. You then
use TOB to do state machine replication, where each message is a
decision.

It's more efficient to use consensus to do leader election, then just
use primary replication.

## Ch10: Batch Processing

Talks a lot about MapReduce. They talk a lot about all sorts of join
tricks. I'm familiar with most of those.

They talk about how MapReduce can be a good choice for batch producing
search indexes. And also fault tolerance properties of MapReduce
framework.

They discuss systems like Spark which offer optimization to the
MapReduce model (primarily by not materializing intermediate state).

They discuss Pregel briefly. Pregel is basically like the actor model,
but consisting of phases.

They mention that a more declarative style like in Spark allows for
query optimization. Also, less code.

## Ch11: Stream Processing

**TODO**: I'm summarizing notes up to here!

## Resources

* https://jepsen.io/consistency
    * Nice graph of different consistency models.
