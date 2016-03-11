Garcia-Molina suggests a different version of MVCC than I have
recorded. His version has *two* timestamps on every record: a read and
a write timestamp.

Here's how the basic form works:

* When you start a transaction, you get a transaction ID.
* Before reading a row, check the `WriteTimestamp`; if the write
  timestamp comes after your TXID, then abort.
    * Else, update the read timestamp (if our TID is later) and a
      "commit" bit to false.
* When you write, check both `ReadTimestamp` and `WriteTimestamp`; if
  you're greater than both, do your write and update the
  `WriteTimestamp`.
    * Again, set the commit bit to false.
* If TXID<RT, but WT>TXIDnot:
    * If a later write has been written there, we can ignore this
      write, since its already been replaced. And no one has tried to
      read us here, so who cares.
    * If that write hasn't committed, we need to wait. That's because
      if the other writer is rolled back, we need to put *our* value
      in.
* If TXID<RT, then you have to abort.

This is concurrency control with timestamps, but no multiple
versions. Note; I think you need to *re-issue* a TXID write before
commit, and double check this work one last time. Otherwise, there's
no way to check for concurrent modifications?

Sometimes it is okay to read older versions. Let's consider what would
we could do if we created a new version on every write:

* Write times never change. Writes can't go through if they have a
  TXID less than the previous read.
* Reads look for the latest version before the TXID.

So let's say you start a read only transaction. You will try with the
current TXID. You read all the relevant data. You go to complete the
transaction. Everything you read is *still valid* for that TXID. You
complete without blocking.

Let's say you have a transaction which reads some data and writes some
data. You get a preliminary TXID. You read the data appropriate for
that TXID, and you want to make some writes. You go to finalize the
TX, you issue the TXID it *will* commit with. You check: was any of
the data you read modified by a transaction in between?

Effectively, this is my materializing the conflict suggestion. It
seems like with this suggestion transactions should only abort if they
actually really do overlap. It doesn't reduce reader throughput at
all.

Garcia-Molina goes so far as to say that some DBs use a hybrid where
read-only transactions use MVCC, while others fall back to two phase
locking. This can have better throughput, because aborted transactions
are painful. But then I guess you do have the possibility of
deadlock...

I think that the old way I describe MVCC acheives *snapshot
isolation*, which *is not* serializability. The way I've described
*does*. I think it aborts basically the minimum number of
transactions.

I think that Postgresql doesn't really use this method, though. I
think it uses serializable snapshot isolation; using the same kinds of
timestamps and rules, but just additionally runs a cyclic dependency
checker. I think the advantage to this would be no deadlock...

## TXID?

I think if you use a timestamp instead of TXID things are maybe
clearer. You just need monotonic time.

Again, read only queries can operate with last committed
timestamp. They don't have to block anyone.

When you write, you get a timestamp. As you read records, you update
read timestamps. Read the most up-to-date version of what exists at
the time of your timestamp. It's okay, you can allow later writers to
write over stuff that you read.

The only thing you can't do is write something that they haven't
read. So if you want to write something, and you see that a committed
transaction with a higher TXID has already read the old version of
this data, you have to die. You no longer could have taken place
before the other TX.

If the TX is in progress, you have a choice: you can die, or they
can. Maybe they should die; that seems fair because they have the
higher TXID, and livelock is more likely if you kill them. OTOH, it
could be hard to stop them if they're in the midst of committing. For
today's purpose, I don't care.

Let's consider from the other side: what if you see a write from
before you TXID? As ever, if it's committted, just go ahead and read
it (marking, obviously). If it's not committted, you need to wait,
because who knows if that's going to actually be the real record.

Note that this waiting *cannot cause deadlock*, because you only wait
on older concurrent transactions.

(I believe this is what Garcia-Molina really says!).

The good news about this strategy is that it should avoid deadlock,
but also be serializable. It shouldn't kill any transactions, except
ones which *truly are unsafe*: where you're trying to do a write
before a committed transaction that already read that data and
returned. So only conflicting transactions get delayed.

The rules can be enforced without a "final check" at the end. You
check along the way, kind of like locking. That's reassuring.

I think I also learned how predicate locks work. Basically, if your
predicate is an index, you just touch the readstamp of the index. That
stops someone from inserting records at the values you are protecting.

## So Why SSI?

The system I described aborts some transactions that could be
processed simultaneously. Let's say T1 gets a lower TXID than T2, and
tries to write a row that T2 reads.

If T1 commits before T2, then everything is fine.

If T2 commits and then T1 tries to write the record T2 read, then T1
needs to abort (under the scheme I've described); T2 couldn't have
happened at the later timestamp if T1 is supposed to happen at the
earlier timestamp.

But what if we could have reorderd T1, on the fly, so that it got a
higher timestamp than T2? It's possible that T1 isn't going to try to
read anything that T2 wrote, so it could run concurrently to T2, and
not worry whether its write got committed before T1's read or came
after.

Likewise, if T2 sees an uncommitted write from T1, what should it do?
It can wait for T1, but that could take forever. If T1 just hangs,
then T2 could be wedged. T2 could try to kill T1, but that could lose
resources. Again, if T2 isn't going to write anything that T1 sees, it
could just go ahead and read the old record, and doesn't need to
coordinate with T1; T1's write could happen later.

Basically: if write skew doesn't happen, then we could have let these
transactions run concurrently without killing or waiting. Basically,
we would be letting T2 get "reordered" before T1. NB: if T1 actually
commits before T1, we could temporarily observe an anomoly, but
eventually that would heal itself.

So we see that we could get more throughput, but at the cost of
serializability.

I believe this is what SSI is trying to do. It's trying to allow these
kinds of transactions to run concurrently, but *without* the danger
posed by write skew. It does this by maintaining a graph and
eliminating transactions that get dangerous.

It's more optimistic. If the danger never occurs, then no one needs to
rollback. But it's a more complex scheme, of course.
