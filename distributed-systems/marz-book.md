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
