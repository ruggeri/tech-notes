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
