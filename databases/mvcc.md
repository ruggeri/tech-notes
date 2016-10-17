## MVCC/SI Tradeoffs

So we know that MVCC can cause transactions to be aborted
unnecessarily. In particular, a transaction can do a lot of work, but
because it doesn't lock anything, a short transaction can swoop down
and change one piece of underlying data, and cause the longer
transaction to abort.

To avoid that, we can use SI to reduce aborts, but that has the
downside that it lets write-skew anomolies.

I believe Garcia-Molina suggests a potential approach which is a
hybrid of locking and MVCC, where read-only transactions can read old
versions, but writers user locking. They suggest this is used in some
commercial systems.

Another approach is to try to recognize transactions that are true
write-skew, rather than ones that overlap in a trivial way (as
above). This involves monitoring a dependency graph for conflicts. In
particular, Cahill et al show how to detect the potential start of a
write-skew. This has false positives, but is generally accurate, and
easy to test for.

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
