These are notes from the Nathan Marz book.

## Ch1: Introduction

Say you want to keep track of pageviews per URL. You can do this with
a SQL db. But then as write load increases, you might start to timeout
your writes to the DB. To deal with this, you might use a message
queue, and then have a worker do a batch of updates. This also
eliminates a problem with timing out, since the queue is always
available to store the message.

If load continues to increase, then the worker won't be able to keep
up with the updates, even when batching the updates. To deal with
this, you can add more workers, but at some point the DB just can't
handle the write load.

To deal with that, you can start sharding by key. That distributes the
load. As you increase the number of shards, you start to have problems
where DB servers go down. To deal with this, you can save the update
message for the down machines to a "pending" queue, and try to flush
this when the machine comes back up.

You can have read followers so that even when down, you can still
get at the data. But you won't have availability for writes.

You could have a bug where you accidentally add some corrupted
data. To undo this bug, you could restore a backup, but that will lose
data. Now you wish that the DB had an immutable log of
updates. (Actually, I think that most DBs do this with the transaction
log, right?).

And right now you aren't trying to do any multi-row operations!

Desired properties:

* Fault tolerant: machine faults, or software errors.
* Low latency reads/updates.
    * Most systems need low latency reads.
    * Not all need low latency updates, but a generally useful system
      would have this.
* Scalability by adding more machines.
* Ad hoc queries: important for exploration.

He's really against "incremental state". Basically, the idea that you
maintain and mutate information as you go along. I think he wants
something more immutable, and more of a log.

He notes that there's typically a lot of work in making a system
eventually consistent (becomes consistent when partition heals), which
is a natural result of a business requirement for the system to be
highly available.

It's easy to see this healing is going to be easier with a log of
updates. He gives an example of a counter; if the system goes into
partition, and replicas on both sides of the partition get increments
to the counter, what happens on heal? We don't know what the count was
when the partition happened, so we don't know when they diverged. It's
easy with a log because we can just merge the log files.

His point is that handling a count like this is the easiest thing you
could want to do in a DB, and it's already fucking your life by making
you write a shit-ton of code to deal with this. Shouldn't the system
be taking care of this?

To be resilient to human errors, he recommends a log, which can be
kept even with an incrementally updated DB by adding a log.

Says that queries are a pure function of data. Don't want to read all
data for every query, so we do precomputation. You can even precompute
subproblems needed for ansering a query. E.g., if you want to find out
how many views of a URL over `n` days, you might have already computed
the number of views for each day, making this problem much easier.

He calls this the *batch* layer. Obviously the batched results will be
out of date, which could be a problem (or maybe not). Anyway, we'll
later fix it so we get up-to-the-minute results. The batch layer has
immutable data created in an append-only way, and the calculation of
results is pure. They suggest doing this batch work with Hadoop; this
means you can get distribution done for you for free. (Note: not every
problem is best solved with Hadoop, but they don't mention that).

We then run a read-only database on these batch results. They use a DB
called ElephantDB. This looks like a really simple key-value store,
and you lookup batch results by name.

This is pretty good: they claim ad hoc queries are easy, but I would
debate that. You can only ad-hoc query from the batched results, which
could be limiting. But provided your batched results are robust, no
worries.

To be able to use the most recent data, there's a *speed* layer. This
keeps the data that has been ingested since the last batch
finished. Only here will we do incremental update of what is
effectively a materialized view.

One trick: it might be hard to do an incremental update in the speed
layer. In that case, you can use an approximation algorithm
there. Your old batch results are perfectly accurate, while the speed
layer may be slightly inaccurate. They mention HyperLogLog for user
counts.

He calls this the *Lambda Architecture*.

## Part 1: Batch Layer

Says store rawest data you can, often unstructured. Don't try to
normalize or extract information at this level. Basically log
everything. He also says don't take it too extreme; obviously useless
data cna be ignored. Just err on the side of not throwing out anything
you might conceivably want.

You may want to eventually garbage collect low-value data, or even
censor data for regulatory purposes. He suggests doing a batch job to
filter, and then rewriting the data set. That way you can perform
tests before you delete the old dataset.

Suggests storing bits of information called *facts*. A fact could be a
modification to a friendlist, or a page view. It's the minimal amount
of information to describe the event.

Thrift. Basically provides a schema and storage format. Enforces
schema. Doesn't enforce other validation logic; you have to write that
yourself.

Use HDFS to store, as you need efficient append, scalable storage, and
parallel querying. They say don't bother with key-value, because most
key-value stores are going to support random operations that you don't
need or want.

At the batch layer, you can just store all your data in distributed
log files. Or you might try to do some partioning to help out
performing the batch computations. E.g., to split up data by hour, or
to organize by user id, or something. This is just an optimization.

Okay, let's start boiling the data down into views. Obviously we want
to boil down into useful building blocks; we aren't always able to
precompute an exact answer to every query. To do this, we can either
*recompute* on all the data whenever new data is batched, or use an
incremental algorithm that will take in the old result and the new
batch data and produce a new value. Incremental tends to be nicer.

Talks about what Hadoop is; scalable, fault-tolerant. Shouts out to
Spark, which can higher perf because it tries to keep some data in
memory. Mentions that it's a pain to do multistep computation. And
that joins kinda suck to implement.

He suggests that MapReduce, while maybe a good choice at the low
level, could use a higher-level API of a stream:

* `map` and `filter`
* `group` (presumably aggregate)
* `join`
* `merge`

A compiler could take this down to MR. That's what Cascalog is (Marz
made Cascalog). Appears to be built on Cascading; not sure how they
are different. Marz likes Cascalog, because it's just pure Java, so
not a new language; so has full power of Java.

I'm not hyper-interested in Cascalog. It looks kinda shit, actually;
everything is a "predicate", whether it does an aggregation, or a
filter, or whatever. Looks like type-checking won't be possible
AOT. Joins seem to be implicit? Seems kinda shit compared to what I
might have expected.

The general idea makes sense, of course. I'm not interested in the
details anyway.

Okay, so let's talk about an example:

* To support querying views of a URL over a timerange, compute hourly
  stats.
    * Also compute daily and weekly stats, so that you longer queries
      don't require querying more records.
* Unique views over a timerange.
    * Harder, because not additive.
    * Apparently there is a mergeable version of HyperLogLog? Not sure
      how that works.
    * I guess I could see that, since you really only need to keep
      track (in each hash partition within each hour) of the max
      number of leading zeros.
    * Then, to combine hours, you just take the max of the two. This
      is exactly the same as if you did HyperLogLog on the two hours.

**Thought**: this all works very nicely for a system where there is
one kind of event. What about more complicated systems with many kinds
of events? I'm not as sure; I'd like to see this extended beyond
counts of URLs.

## Serving Layer

Because the views are just computed from the underlying data, you can
denormalize to your heart's content, and precompute as much as you
want.

Rather than just a key-value store, they want to do a BigTable type
thing and have a key map to a sorted map. This means that you don't
need to go to many servers, or even do many disk seeks, to get a bunch
of related information about a key. That can be a huge win.

Note: when you need to request from a bunch of servers in parallel,
you're going to tend to make the worst case the common case.

They also mention it's nice if you can have keys structured in a way
where the first part of the key decides the partition, and the second
part is indexed at the server for that partition. They give the
example of `(url, granularity, start-time)`: you want to partition by
URL, then you can find the appropriate level of granularity at that
server, and then you can do a read of the appropriate range.

Serving layer doesn't need random writes, so it's much simpler. In
particular, never need to do any kind of GC, which is going to
increase reliability of performance. Everything's easier by not
allowing incremental updates here, because you don't have to
recalculate computed results from an update, which might actually
affect a *lot* of computed results.

They give an example of unique pageviews, *with* stapled ids. When a
stapling event happens, how can you possibly recompute? Any summary of
the pageviews that doesn't keep the isn't going to be possible to
update. (NB: not sure how the "speed layer" is going to help here...).

Another thought: feels like you're making ad-hoc querying a lot
harder. It might be desirable to do that while developing or
investigating. But you still have a distributed dataset that you can
run Hadoop jobs on, so it's not all bad. It's not like this thing is a
distributed RDBMS like Postgres; maybe you could throw on a tool like
Dremel to help with that. And you're not going to be doing ad-hoc
queries in production.

They try to suggest a solution to the stappling/equiv problem. They
say store the ids for every user who visited a URL aech hour. To get
the number of unique users in a time range, take the union of these
sets. Then, transalate all user ids to "person" ids, which is unique
across stapling events (what, are you going to query each person; that
sounds terribly slow??). Then count these. This is an approximation,
and will probably suck for low volume pages.

Okay, so this definitely sucks. But we haven't seen how the Lambda
architecture is going to help; we've just deferred this discussion
until we look at the speed layer.

He shows how to use ElephantDB for the server layer, but this seems to
be abandonware. Not sure what a better choice would be today.

ElephantDB runs on top of HDFS. You run a MR job where the input is
key/value, and this builds the shards, which are served by the
ElephantDB process. Shards are replicated, so you've got fault
tolerance of the serving layer.

This whole thing sounds like a reinvention of HBase, really. It's less
complicated because it doesn't need writes like HBase, but then again
it's unmaintained...

## Part 3: Speed Layer

You're going to handle updates online by building an incrementally
maintaing a view of the recent data.

You'll still need random reads, but now you also need random
writes. And of course scalability and fault tolerance are
important. The good news is that because the speed layer is only
recent data, it is much smaller.

Sometimes updates are hard to incorporate incrementally. For instance,
when a new stapling event comes in. You can use approximate algorithms
in your speed layer, knowing you'll eventually get the exact answer
later in the batch mode.

Mention CAP theorem: first note is that CAP is about what happens
under partition, so really it's saying your choice is consistency or
availability. When you choose consistency, that means sometimes a
request will return with an error. If you choose availability, then
you'll get either stale data, which basically means that updates are
not linearizable. BTW: availability is defined that all requests to a
non-failing node must be processed; if you're partitioned away from
all replicas, then it is not "unavailable", because you can't even
send a request. So be careful with the language!

Explains that you can lessen availability problem by adding
replicas. But the problem comes down to writes; what do you do when
the partition heals? Some systems even have a "sloppy quorum" feature,
where if all replicas are unavailable, you can create a new
"temporary" replica for updates that will eventually be merged in.

It sounds like he's going to advocate availability in the speed layer;
by picking Cassandra. To handle healing, he suggests CRDTs. CRDTs say:
given a set of updates, applied in any order, you'll get the same
result. This means when you heal, you can merge the updates, and come
to an answer that, in a sense, doesn't roll back any updates. OTOH,
your observations on both sides of the partition before the healing
may not be compatible with any one linear order.

Basically, CRDT lets you get "back on track" in a way that doesn't
"undo" any previous update. Simplest example is a counter.

In general, it is common to do a *read repair* when you next read a
value after a heal. But this logic can be very difficult to get right.

One question is whether to do synchronous or asynchronous updates. A
synchronous update does the update to the DB before acknowledging to
the client. an async update puts the client's message onto a queue,
which will later be processed; that might take a little bit. One
advantage is that async updates can be batched to acheive higher
throughput. Also the queue can even out traffic spikes.

Of course, the update is not immediately reflected if you enqueue
it. So you might prefer sync for a system where updates need to be
immediately viewable to the user. OTOH, Marz recommends a default of
async, for better throughput.

How to expire data from the speed layer when the batch layer completes
its work? Marz recommends to realtime views; the current one, and the
one which starts at the beginning of the most recent batch layer
build. When the batch layer finishes, expire the previous, and create
a new one, starting now (since we immediately start the next batch
layer construction).

Cassandra is suggested for the speed layer datastore. Cassandra is key
to sorted map (like BigTable). It distributes easily, has fault
tolerance. One note: there isn't any discussion of transactionality in
the sense that a single update may need to touch multiple rows. This
is definitely going to present the possibility of straight-up
inconsistency, not just eventual consistency.

He then starts talking about queuing. One queue methodology is to pull
events off a queue, and then either report success or failure; failure
to ack promptly is a failure; I assume you have heartbeats. Of course,
your messages need to be idempotent, as you could update the DB, but
die before sending the ack to the queue.

There may be multiple consumers of the same straem: maybe one team
wants to do X with the stream, while another team wants to do Y. With
a single-consumer queue, all applications must be deployed together;
each time an event is pulled from the queue, it's given to every
consumer application, all wrapped in a single JAR. Also, it's not
clear what to do if one client succeeds to process a message, but
another fails!

To avoid these problems, you could have multiple queues, one per
client consumer: every message should be placed on every queue. But
that is useless duplication and load.

I believe RabbitMQ is an example of a single-consumer queue; it
basically just load balances for you (and presumably persists
messages).

Another approach is a multi-consumer queue. The idea here is that the
queue is a log, and that the queue buffers a bunch of events. The
queue will keep, e.g., 50GB of events. Clients are responsible for
knowing where they are in the stream. No ack/fail is required. And you
can always replay through the events in order. This is what Kafka does
(I think it also distributes the queue and makes it
redundant. Basically, Kafka is like a transaction log that any
application can play through. Marz seems no downside to Kafka versus
RabbitMQ.

So let's talk about stream processing. You can do one-at-a-time, or
micro-batched. The contrast is latency versus throughput. We'll focus
on one-at-a-time first.

First, you might have a pipeline of workers, each worker should have a
queue in front of it. Multiple workers might share a queue, if they
are consuming the same stream. One potential problem is that, if you
divide up work amongst multiple workers, the workers might try to make
conflicting updates. E.g., if you're doing a counter, and you don't
have an atomic increment, i.e., you read and add and set, then only
one worker should be updating one URL. But that is troublesome if the
load isn't truly even.

One problem with having a queue in front of every worker is that this
creates additional load from just copying things around queues.

Also, I must again stress that you have no transactionality across
rows, which I think should be very limiting.

He starts talking about the *Storm model*. Here you have an event
stream (for instance, from Kafka). The origin of the stream is the
*spout*. A *bolt* consumes some input streams, outputing a stream. You
then connect these up in a topology. When a stream is split, it can be
split randomly or by hash of some keys (to make sure all the data for
one key travels the same path).

Storm has a feature where ther are no intermediate queues; if there is
a failure a tuple is started again from the spout. There is some
algorithm to do this, but it is not described here. Note that this
means you may process a message multiple times, which is not
idempotent. That's no different than before. He also mentions that you
might not care about a little temporary inaccuracy if it's rare.

He claims there is a way to do this *exactly once* if you sacrifice
some latency. I believe that is discussed later.

He starts talking about the equiv problem in the speed layer. One
approach is to ignore new equivalencies in the speed layer, and just
use the batch equivalences for counting purposes. This is inaccurate,
but temporarily. Note that not only the speed layer information is
inaccurate for the moment; we're also failing to collapse the users in
the batch layer. In general, though, any inaccuracy is only in the
speed layer, and that's temporary.

It's hard to know what a great solution to the equiv problem is
without storing every hit to every page.

Marz notes that inserts into a HyperLogLog are idempotent, btw.

Marz talks about micro-batching. Here, a small set of tuples are
processed at once. The key is that batches are processed always in the
same order; if any update fails, then the batch is repeated. With
this, you can give each batch an ID. Then you can check the ID before
doing the update, to see if you've already applied this
operation. Note that this doesn't work without ordering.

BTW, a log-like queue such as Kafka is what you want here, so you can
repeat batches. To get parallelism, note that you can partition the
keys in a batch to spread out the work.

One thing that's weird to me. Marz says when you make an update, store
the batch id number alognside. Then if you must repeat, you see the
batch id number. But what if you make several updates to the same key
in that batch? I guess you must not do that...

I really feel like this guy is overselling what is possible/easy with
Storm.

He says it would be scalable to do this on a per-tuple basis. That's
probably correct, because you couldn't do any processing until the
current tuple was done being processed.

BTW: you can do your own batching, collecting up a number of tuples at
a bolt, then doing a batch update. To make sure the tuples don't
timeout and are retried, you should use a timer to collect for at most
1sec or so.

Obviously adds latency, but can also increase throughput.

Gives an example of how you can do batch processing in the speed layer
for bounce rate calculation, which necessarily has a 30min latency
(since you can't detect a bounce until 30min of inactivity). Not that
interesting...

Trident is a Storm API for micro-batching. Specially handles Kafka and
Cassandra.

He mentions one problem: what if a worker has cross-microbatch state
and fails? Then you can retry the microbatch, but you're not going to
get that state back. You could have basically a commit log on that
state. So that you don't need to recover from the beginning, you could
checkpoint. You could then rollback and replay batches from the last
checkpoint.

Apparently Spark Streaming does this.

Note once again that things aren't as easy as he says.

## Wrapup

Can do a partial recompute when you do need to look at old data. This
can help if you don't have to do a sort of the whole data, and can do
a map-only job. When filtering the data you could use a bloom filter
for this. You then have to rebuild the view; taking the old view and
the recomputed portion. That sounds like another MR job. Of course,
the bigger the view, the more just copying around you have to do.

One possibility is to have tiers of batch views. The base tier is
recomputed every once in a while, while more frequently a faster tier
is partially recomputed more frequently.

A thought on scalability: say you decrease cluster size by 10%. Will
the batch layer be recalculated 10% more slowly? Actually more, since
in the time it is calculating, you'll have more data collecting for
the *next* recalculation. If you decrease the cluster size too much,
you won't keep up with new data, and you'll start falling behind.

OTOH, an increase of 10% resources to the cluster can have a greater
than 10% improvement, by the same logic.
