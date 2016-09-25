## Vertical Scaling

Add a ton of memory, more disks in RAID. Have multiple sockets on a
motherboard; use multi core processors. Use InfiniBand on a rack of
such machines.

## Read Slaves

First you set up read slaves. Thse can't accept writes, but can accept
reads. You're scaling horizontally here. You can do synchronous read
slaves, in which case you may have a "submarine" write to the
slave. Or you can do async, which means you might lose very recent
committed writes if the master goes down. But note that there's only
one serializable transaction order, so even with async log shipping,
there's no possibility of inconsistency.

Be careful with promotion, by the way. Even if a write is confirmed on
a quorum of other machines, it can be lost if the new master doesn't
get this write.

For a read-heavy workload, this increases your read IO, but doesn't
increase write IO (if anything it slows it down). It doesn't increase
your disk capacity.

It increases redundancy; if a machine fails, you can promote a read
slave to a write slave. You could do this automatically with
Paxos/Raft.

I think that async slaves is more common, because that acheives higher
throughput, and has an acceptable loss of writes. I believe it is
typical to do log shipping, which specifies a serial order. Seems like
a good idea, since only the writes need to be applied; we don't need
to recalculate the result.

## Sharding

Read slaves don't help you increase disk capacity, or write
throughput. For that, you can shard your tables, hashing the keys and
distributing them across shards.

This is going to increase complexity, so you really do want to avoid
this. You may be forced to shard if you have a very large working set
in memory, or you have too much write IO.

First, this makes querying harder, since data is spread over many
machines. Full table joins are going to involve communication between
every machine. But maybe you shouldn't be doing full table joins like
that.

The other problem is when you want to make updates to items located on
different parts of the DB. You're going to have to do a distributed
transaction, which is complicated, and can cause deadlock (especially
in the presence of machine failure).
