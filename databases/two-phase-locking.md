# Database Concurrency and Transactionality

The fundamental difficulty of databases is the conflict of concurrency
(the ability to process multiple requests simultaneously) and
isolation/consistency.

If concurrency were not required, we could process SQL transactions
serially: the next transaction cannot begin until the previous one
finishes. Since each transaction should not commit until the database
is left in a consistent state, the next transaction should always
start with the database in a consistent state. In the serial world, we
need only prove that, provided at the outset of a transaction the DB
is in a consistent state, the INSERTs/UPDATEs performed by the
transaction are such that the DB invariants are maintained by end of
the transaction.

Serial processing reduces throughput. It is desirable that orthogonal
transactions be processed concurrently. For instance:

* Two transactions that only READ (i.e., neither WRITEs) can be
  processed concurrently.
* Two transactions that WRITE different data can often be processed
  concurrently (hold on for more discussion).

The trouble is that identifying transactions that can be processed
concurrently is difficult. We need to get this right because otherwise
concurrency can cause a loss of consistency in the DB.

## DB Locking and Isolation Levels

One solution to handling concurrent query execution is to use
locks.

When a row is **locked**, we forbid other updates or deletes to the
row until the lock is released. Whenever one transaction accesses a
row, it can lock it so that another transaction cannot modify
it. Let's consider an **isolation level** where a row is locked by a
transaction whenever it is read or written. The lock is not released
until the transaction ends.

Two transactions that read/modify different rows can now be processed
concurrently; neither desires a lock that the other transaction
claims. Additionally, if two transactions both try to read the same
row, they should both be able to acquire a read lock; read locks
should not block other read locks, they should stack.

### Uncommitted Reads

Writes locks should block acquisition of simultaneous read/write
locks. Write locks must exclude other write locks, of
course. Otherwise:

```
T1 begins
T1 writes row R1
T2 begins
T2 writes row R1
T2 writes row R2
T2 commits
T1 writes row R2
T1 commits
```

Here the state is left with a mix of two updates; R1 is update per T2,
but R2 is updated per T1. So writes should block other writes.

Writes must also block read lock acquisition, otherwise:

```
T1 Begins
T1 Acquires write lock on row R
T1 Modifies row R
T2 Begins
T2 Acquires read lock on row R
T2 Reads row R, sees updated data (!!)
T1 Aborts
T2 Proceeds with the uncommitted data
```

This **anomoly** is called an **uncommitted read** (also sometimes
called a **dirty read**). Data is read before it is committed to the
DB. This is particularly troublesome if T1 needs to make multiple
updates to the DB, all of which are supposed to be simultaneously
committed. The ability to see changes from an in-progress transaction
means that a transaction can see an inconsistent view of the DB.

If write locks do not block other reads/writes, then the isolation
level is called **read uncommitted**, because uncommitted reads can
occur. If the DB enforces that writes do block other reads/writes,
this is called **read committed**, because reads can only read
committed data.

### Non-repeatable reads

Anomalies can still occur at the read committed isolatation
level. Let's see what happens if read locks don't exclude write lock
acquisition:

```
T1 begins
T1 acquires read lock on row R
T2 begins
T2 acquires write lock on row R
T2 Modifies row R
T2 commits
T1 Reads row R again (!!), gets different result
```

This is called a **non-repeatable read**. If the transactions were
processed serially, such a result could not happen. The mode where
read locks block is called **repeatable read**.

Note that in this scenario that T1 only sees committed data; the
problem is that because T2 commits during the execution of T1, T1 sees
data both **before** and **after** T1 commits. So it doesn't have a
stable view of the data.

To prevent the non-repeatable read, read locks must be obtained and
block write locks. Read locks don't block further read locks.

### Phantom reads

This does not end our sojurn toward **serializability**, the property
that transactions are processed **as if they had been serialized in
some order**. Serializability allows for the concurrent processing of
transactions, but it also requires that the result of processing a set
of transactions is consistent with having processed them serially in
some order. Serializability is the holy grail.

The next problem is the **phantom read**. Let's see it:

```
T1 begins
T1 selects all rows matching predicate P; it locks these
T2 begins
T2 inserts a new row matching predicate P
T2 ends
T1 repeats the previous select; it gets a different set
```

This adds a new complication. It isn't enough to lock rows; we need
locks on a query; any operation that changes the set must be
blocked. This adds some complexity to the DB, in the sense that T1
doesn't know a-priori what locks it has to wait on. The DB will have
to examine the current SELECT locks and wait for those to clear.

These are called predicate locks.

Traditionally, if such locking exists, serializability is achieved.

## Two phase locking

In the preceding discussion, we obtained locks and released them only
when the transaction finished committing. This implementation (lock as
statements execute, release when transaction commits) is called
**two-phase locking**.

There are two primary difficulties with two-phase locking. First is
the possibility of **deadlock**. If T1 and T2 both need to acquire
locks on rows R1 and R2, but acquire them in a different order (T1: R1
then R2, T2: R2 then R1), there is a possibility that each of T1 and
T2 will acquire the first needed lock, but be unable to acquire the
second to complete. Neither transaction can progress.

This requires aborting and re-trying one of the transactions. The DB
has a **deadlock detector** which looks for deadlock, aborts one of
the transactions, and completes the other. The aborted transaction can
then be retried. We won't discuss this further at this time.

The second problem is that serializable mode can suffer low
throughput. For instance, say T1 locks a row R. T2 only wishes to read
this row R. T2 cannot read the row until T1 completes, which may be a
while. In the meantime T1 has to wait to acquire the lock.
