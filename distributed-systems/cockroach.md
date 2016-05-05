## Atomicity

* Row-level locking.
* To edit a row, you put in place a switch with references the
  transaction ID.
* The transaction ID is itself stored in the DB, especially whether it
  commit.
* All switches are "flipped" when the transaction ID is commited.

Therefore it is "atomic"; everything goes through or does
not.

However, non-repeatable reads would be a problem. Presumably this
could be resolved through some kind of MVCC situation. Reads are also
slow in that they require a 2nd disk hit to look at whether a
transaction is committed. So we have potentially serious consistency
problems.

## Consistency

Every transaction is going to get a timestamp at its start. We're
going to keep a serializability graph (this is the RW, WR, WW
thing). We won't allow an edge to be added that says that a lower
timestamped transaction `T1` should happen after a higher timestamped
transaction `T2`. Basically: no operation of T1 is allowed to take
place after a T2 change. If that happens, we kill T1 and must
retry. This should keep the graph acyclic obviously.

Now, no WR edge can be added where a lower timestamped transaction
reads something a higher timestamped transaction wrote. That's because
we have MVCC, and we can just read an earlier row. This is nice,
because we don't have to immediately kill the transaction.

When a read happens at a node, we update a cache of read
timestamps. When a write comes in, we check the cache; if an lower
timestamp transaction has read this row, than the write from the
higher timestamp transaction represents a RW edge, and we must kill
the high transaction.

To prevent the the phantom read problem, this is an *interval*
cache. Basically, this divides the keyspace into pages, and you touch
every page you look at for a read. This is like how we used an index
to only lock parts of a table.

They use an LRU for this. We keep track of the oldest timestamp in the
cache, so in the case of a write with a cache miss lookup for the last
read timestamp, we can just assume it happened before that read.

We just do the same thing with WW conflicts.

By maintaining this (which is just optimistic concurrency control of
course), we know we maintain a serializable schedule at all times.

## Recoverability with Strict Scheduling

This is a problem where, if a lower timestamped transaction T1 writes
X, which a higher timestamped transaction T2 reads, what happens if T2
commits before T1? Can T2 use the written version of X? What if T1 is
aborted? But then again, what if not?

We say that we cannot maintain a *recoverable* schedule in the face of
aborts.

So we use *strict scheduling*, which says that we will only ever read
*committed* values.

How do we acheive this? Well, we encounter an uncommitted value when
we encounter an *intent* which is just a pointer to the transaction
switch we talked about from the atomicity chapter. We have to worry
about that if (1) we are doing a read and the write intent is from a
lower timestamp mtransaction or (2) we are doing a write (whether this
write happened in a lower or higher transaction is equally worrisome).

So we look up the transaction; if the transaction is committed, then
we clean up the intent, reflecting that it is committed, and cary on
as normal. OTOH, if the transactionw as aborted, we remove the intent
entirely, again continuing as normal.

At this point, it looks like one of the transactions has to either
wait, or one has to abort. They have a way to decide which side should
abort (it's not always one or the other).

## Thought About Canceling Transactions

What if we look at everything and try to cancel the transaction. But
then they commit right before we cancel them? Now it's us that should
get canceled!

I suspect this isn't a problem, because the transaction is handled at
one site, so operations on this transaction are explicitly serialized.

## Resources

https://www.cockroachlabs.com/blog/how-cockroachdb-distributes-atomic-transactions/
https://news.ycombinator.com/item?id=11630674
https://www.cockroachlabs.com/blog/serializable-lockless-distributed-isolation-cockroachdb/
