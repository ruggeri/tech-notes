## Timestamp Concurrency Control

Here's how the basic form of timestamping works, with no versions:

* When you start a transaction, you get a transaction ID.
* Before reading a row, check that WT<TXID.
    * If not, then must abort the transaction.
    * If so, but commit bit is false, you need to wait. You don't know
      whether this row will be written.
    * If so, update the read timestamp (if our TID is later).
* When you write, check if RT<TXID and WT<TXID.
    * Then you can do the write.
    * Again, set the commit bit to false until you commit.
* If writing but RT<TXID and TXID<WT:
    * That means your write is going is supposed to be forgotten.
    * If the commit bit is set, then you can just continue.
    * Otherwise, you have to wait. You don't know whether the write
      already in place will actually be committed.
        * If the in-place write gets aborted, your write should be
          here!
* If writing and TXID<RT, then you have to abort.

Right now this is basically no better than locking, really. You have
readers blocking to wait for concurrent writers. In fact, you can have
readers *abort* because of concurrent writes. Also, writers now don't
wait for readers; they must abort.

There is a slight optimiztation, where T1 can read, and T2 can write
(assuming TXID1<TXID2) before T1 finishes. T1 encounters no problem if
it doesn't try to re-read the row. That is, there is a slight
advantage maybe over 2PL.

## MVCC Part I

In our version of timestamping thus far, reading transactions must
abort if a write has already happend with a later TXID. We could avoid
doing that if we kept older versions of records.

So instead of overwriting any records, create copies. The read
timestamps will monotonically increase, but the write timestamp never
changes.

Now, a read can use the latest version that was valid at its TXID. As
before, a later read must wait on an uncommited write. So writers do
still block approximately half the readers (depends on who got the
lower TXID).

A write must abort if a later read happened on the most recent
version. But it now doesn't need to wait on an uncommited later write;
it can just put the record in place!

This is an improvement. We now never need to abort a reading
transaction. However, a reading transaction may still have to *wait*
on a concurrent writing transaction to commit. More, writers must
still abort if interfered with by a concurrent reader.

## MVCC Part II: Validation

To avoid needlessly delaying readers for pending writes, or for
aborting writers because of read-only transactions, let's make sure
that all concurrent read-only transactions are ordered before a
concurrent writing transaction.

To do this, we will only issue new, incrementing TXIDs to writers. A
reader gets a TXID of one before the earliest concurrent writer.

All transactions start out as readers. Even writing transactions just
prepare their writes "on the side."

By nature of the way that TXID is handed out, readers don't need to
worry about anyone trying to insert a new record before their
read. They are never delayed. (Technically, readers don't even need to
check or update timestamps! They will never experience conflicts, or
cause them).

When writers are done preparing their changes, they then need to
validate before writing. First, they request a new TXID. Then they go
through the rows they read/wrote. They will lock each of these
briefly.

If they are reading the record, they should update the read
timestamp. If a concurrent write wrote the record prior to the read,
we should abort, which means going back through the rows, reversing
our changes, and releasing the locks.

If we wrote the record, but see a later read of the prior record, we
should again abort.

Otherwise, we can release the locks iteratively to complete our
changes.

What we have done is make sure that all concurrent read-only
transactions are ordered before all concurrent writing
transactions. This means writers never have to abort for read-only
transactions, and read-only transactions are never delayed by writers.

Writers can continue to kill each other. In fact, we've made it more
costly when conflicts do happen, because every transaction runs to
completion on a snapshot, but the result may be thrown away. There is
no early termination.

## MVCC Part III: Optimizations, Snapshot Isolation

First, if a transaction knows that it will be a writer, it can kill
itself immediately if it wants to read a record, but sees a concurrent
writer has committed a new version. Carrying on would be useless
work. We don't want read-only transactions to do this, obviously, so
we should default optimize for read-only transactions, but then give
the user a way to hint us.

This is better, but it doesn't prevent all late detection of
conflicts, because one transaction needs to commit before the other is
aware of any potential problem. Also, this won't always work (if we
read first, and then an intervening transaction writes before we go to
commit). There is no cureall!

Another optimization is to loosen serializability to *snapshot
isolation*. In this model, we stop using read timestamps; we won't
care if T1 is supposed to come after T2 according txid, but T1 read
data that T2 wrote. We'll only try abort on write-write conflicts; a
transaction will abort only if it is assigned a lower txid but tries
to write a record that was updated by a higher txid.

## Write Skew

What can now happen is *write skew*. T1 reads A and B and writes B. T2
reads A and B and writes A. They can both operate with the same
versions of A and B, even though this is not serializable behavior.

In our old version where we tracked read timestamps, this is not
possible. Assume, WLOG, that T1 is assigned a TXID < T2. If T1 is
committed, it has updated the write timestamp of B, so that T2
realizes it cannot have read B properly. Likewise, if T2 is committed,
it updates the read timestamp of B, so that T1 realizes it cannot
write B.

Not everything aborted by MVCC with timestamps is write skew, of
course. Consider if T1 reads A (and writes something else, like Z) and
T2 writes A. If T1 is assigned an earlier txid than T2, we are
fine. But if T1 is assigned a txid after T2, one of these is going to
have to abort. Under SI we won't abort.

## Promotion

So we can achieve serializability using MVCC. Or we can loosen
isolation and allow in write skew.

Note: the serializable MVCC is basically just the snapshot isolation
approach where we promote all reads to writes. So instead of talking
about "serializable MVCC", I'll just say "SI with promotion." Because
there's another way to achieve serializability with MVCC.

That's not actually true, really! If an earlier transaction reads
something, it shouldn't conflict with a later transaction reading. But
if we treat all reads as writes, then yes this will pose a problem!

## Serializable Snapshot Isolation

We showed that SI with promotion will kill some perfectly okay
transactions, just because we assigned some txids in an unfortunate
order.

Serializable Snapshot Isolation tries to avoid the write skew
transactions, but not unnecessarily kill overlapping transactions. It
does this by maintaining a graph of dependencies and eliminating
transactions that get dangerous. SSI is described elsewhere in my
notes.
