## MVCC

**Multi-version concurrency control** increases concurrency by not
blocking reads with writes. Here is a common implementation:

* Every transaction has a snapshot id indicating the most recent
  committed transaction at transaction start.
* Each record has two timestamps: xmin and xmax.
    * xmin is the transaction id that created the record.
    * xmax is the transaction id that obsolesced the record.
    * xmax of course can be NULL.
* Updates don't mutate records; they set xmax on the old record and
  create a new record with the appropriate xmin (DELETE doesn't create
  a record of course).
* Look only at the last record where `xmin <= snapshot_id && xmax IS
  NULL || snapshot_id < xmax`.
* When committing, issue a new transaction id, use this when setting
  xmin/xmax.

This means you always operate with a consistent view of the database
at some point in time. Mutations due to concurrent transactions cannot
leak into our view of the DB.

However, anomolies can exist. Imagine two transactions want to run
simultaneously, incrementing a user's account balance by $100. How do
we prevent one from clobbering the other? Put another way: if they
were serialized, they couldn't *both* see the same value of the
account balance; whichever transaction is "second" should see the
updates from the "first" transaction.

Note that this can't happen with locking. That's because locking
enforces that data used in a transaction T1 cannot be modified by any
other transaction until T1 commits. Locking is forcing people who want
to mutate data used by T1 to serialize after T1. The only concurrency
is between two transactions with no overlapping access, or between two
transactions that only have overlapping reads. Otherwise one is
blocked until the other commits. The authors of the SQL standard
didn't forsee anomolies like write-skew, because they thought in terms
of lock-based systems where it wasn't possible.

If I think about it, there are a couple restrictions to ordering:

* If T2 reads a record that T1 writes, then it needs to see
  *everything* that T1 wrote. This is enforced by MVCC because T2 only
  reads T1's writes if it operates at a higher snapshot id, which
  means it sees everything done by T1.
* If T2 reads a record that T1 writes, then T1 must *see nothing* that
  T2 writes. Snapshot isolation ensures this because T2 can't see any
  of T1's writes until T1 has committed, which is after T1 would have
  queried all of its data.
* The same arguments go for if T2 overwrites a record that T1 wrote.
* If T1 reads a record and *does not* see an update that T2 writes,
  then it needs to see *nothing* that T2 writes. Again, this is
  acheived by snapshot isolation, because T1 only ignores T2's writes
  if it operates at a lower snapshot id.
* If T1 reads a record and *does not* see an update that T2 writes,
  then T2 needs to see *all of* the updates from T1.
    * This *is not* ensured by snapshot isolation. Two concurrent
      transactions won't see each others' updates. This is the write
      skew problem.
    * NB: Even if T2 does not look at any records T1 wrote, this
      transitively applies to any transaction T3 that sees any writes
      from T2.

## Aborting MVCC Transactions

Under MVCC, we see that write skew anomolies can happen. In a case
where T1 reads an old record and doesn't see a write that T2 makes,
MVCC admits the possibility that T2 won't see the writes that T1
made. If T2 has committed, then ideally T1 would be aborted if it
tries to write something that T2 would have read.

This is not what MVCC does. In MVCC, T1 will be aborted only if it
tries to *write* a record that T2 *wrote*. The isolation provided by
MVCC -- every transaction runs against a consistent view of the DB,
write-write conflicts cause a transaction to abort -- is called
*snapshot isolation*.

Aborting these write-write conflicts seems silly, because blind-writes
don't impose any happens-before requirement on the ordering of the
transactions. Aborting transactions like this (1) aborts some
transactions that could have been safely commited in a serializable
way AND (2) commits some transactions in a non-serializable way.

However, I might defend SI's choice to abort for the following
reasons:

* Most write-write conflicts probably do reflect potential write-skew
  problems. If T2 tries to overwrite a record that T1 wrote, then it
  seems highly likely that T2 might also be writing that T1
  considered. Note that this is trivially the case if the writes are
  not blind. So a very high percentage of write-write conflicts are
  likely to be dangerous.
* It is easy to detect write-write conflicts; at transaction commit
  time, you're going to write a record, so it's simple to look to see
  if the current version of that record changed.
* Avoiding write skew feels hard unless readers block writers.

## Materializing the Conflict

There are a couple solutions to the write skew anomaly. One hack is to
write read records with the same data so as to confirm that their
value did not change over the course of the transaction.

That doesn't seem entirely hacky, actually. Say that writers always
re-wrote all the rows they read. This doesn't block any readers, since
writers don't block readers.

It would cause the transaction to fail if someone in the meantime
committed a write to one of the rows they read. That seems *entirely
appropriate*; in that case, the database already exists in a new
state. Someone could be querying *right now* for data in this new
state. You are about to make a commit based on the *old state*. This
sounds entirely bogus, non-linearizable, and completely observable.

Note that, if the rows you read *are the exact same* as the rows when
you go to commit, then you don't need to rollback the transaction. So
you don't have to rollback when you're doing a write to the same value
you observed in the first place (note this is a little complicated if
you read a row at an old value, and are writing it at a new value that
happens to be the same as the concurrently updated value).

I guess the major problem I see is with phantom inserts. How are these
handled? It seems like you could just buffer these; if any writes
commit that match your criteria after the transaction, then kill the
transaction. That doesn't really seem that hard. But it is, because
it's prolly really hard to see if you would change the result of a
query by the addition of a row. I think the typical thing to do here
is a table lock.

I mean, think of how hard a predicate lock would be to implement. Say
you're joining tables `X` and `Y`. If you add a row to `X`, are you
going to search through `Y` to see if someone matches? Or should you
keep all the joined keys of `Y` in memory for the lock? Or will you
just have locked the entire X table, which is really the only feasable
thing to do...

Given that, are you going to kill a transaction doing a table read of
`X` just because one row got added? At least with locking you could
pause that insert; with MVCC like I've said, you'd throw away all the
work. That's why I *don't* think the default should be to try to
serialize with MVCC.

I think you can do index locking though, which means you don't have to
do a full predicate lock. So that might kind of save things.

The problem I see is that now the MVCC regimen is so strict that you
might have trouble committing transactions; you might go through
multiple failures, which would not have resulted if you did
locking. Basically, you could do a lot of work, just to see it
killed by a trivial task that could have waited a moment.

I think that a smart scheduler could see that and make an intelligent
choice of who to kill.

## Serializable Snapshot Isolation

To acheive serializabile snapshot isolation, we need to add
carefullness that prohibits non-serializable transaction
execution. The first research in this area is by Cahill:

https://drive.google.com/a/self-loop.com/file/d/0B9GCVTp_FHJIcEVyZVdDWEpYYXVVbFVDWElrYUV0NHFhU2Fv/edit

We consider a graph of dependencies between committed transactions. We
add three kinds of edges `ww`, `wr`, `rw`; these are added if `T1`
writes a record, then `T2` writes a later version, `T1` writes a
record, than `T2` reads a later version, etc.

If no cycles exist in the dependency graph, than we can construct a
serial ordering of the transactions compatible with the graph. If
there is a cycle, serializability is violated.

As a lemma, note that `ww` and `wr` both require that the source
transaction commited *prior* to the start of the target
transaction.

Let us consider a cycle. Consider the transaction in the cycle that
started with the lowest snapshot id; call it T1. Note that it must
necessarily have a `rw` edge coming in; otherwise the predecessor (we
call it T0) in the cycle would have finished before T1, and we picked
T1 so that it was the first transaction to start.

I wish to prove that there is some transaction with an `rw` edge in
and a second `rw` edge out. If T1 has an `rw` out, then I am done.

Let us consider the second case: assume that T1 has a `wr` or `ww`
edge out. Then note that the successor to T1 (T2) must start *after*
T1 commits, whereas T0 must start *before* T1 commits. I attempt to
prove that T0 must have a `rw` edge in.

Note that T2 cannot have an `wr` or `ww` edge into any transaction
that starts before T2 starts. So T2 cannot have a `wr` or `ww` edge
into T0, or any predecessor of T0 that preceedes via a chain of `wr`
or `ww` edges, since these predecessors start even further before. But
T2 cannot have an `rw` edge into predecessors of `T0` via chains of
`wr`/`ww` because these must terminate before T0 starts, which is
before T2 starts.

By this logic, T2 may have a `rw` edge into T0. Or, if not, it cannot
have any edge into a chain of `wr`/`ww` occuring before T0. If we
consider a predecessor of T0 that is connected via a `rw` edge to the
`wr`/`ww` predecessors of T0, then it clearly starts before T2 starts,
so the same argument regarding the connectivity of T2 to this new
transaction.

So we just repeat the same argument again.

Boy, I've really belabored this.

## Serializable Snapshot Isolation

To acheive serializable SI we must detect that committing a
transaction could create a cycle of dependencies, and abort the
transaction instead of committing it.

We've shown that a cycle necessarily involves a vertex with an `rw`
edge in and another out. We can try aborting transactions would create
a second edge like this.

The way to do this is to keep `inConflict` and `outConflict` flags for
each transaction. Whenever we read a record that is old, we set
`outConflict` for the transaction, and we also set `inConflict` for
the transactions that wrote the new records. If the writer of the new
record already was committed and had an `outConflict`, we abort this
transaction.

What if the write comes after the read? For every record we read, we
lock the record; the lock won't block writers, but will simply be used
to indicate what transactions have read the record in the past. We'll
look at the lock's owner; we would set `outConflict` on the owner, and
if it was committed, we would abort our transaction.

A difficulty is that a transaction lock cannot be released until all
concurrent with the transaction are finished. But we can just cleanup
old locks every once in a while.

## Further Notes

PostgreSQL effectively uses this algorithm. We didn't talk about
predicate locks; note that when we acquire a SIREAD lock, we might
need to be taking a predicate lock, since it there is an `rw`
dependency between a transaction that *would have read* a row and a
transaction that added that row.

Performance is expected to be better than 2PL, since there is still no
blocking. However, abort rate is higher. Performance is not as good as
SI.

Predicate locks had to be implemented in PostgreSQL for this purpose.

https://wiki.postgresql.org/wiki/Serializable
