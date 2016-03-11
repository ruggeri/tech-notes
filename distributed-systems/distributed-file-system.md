This is a summary of the [Google File System][gfs-pdf].

[gfs-pdf]: http://static.googleusercontent.com/media/research.google.com/en/us/archive/gfs-sosp2003.pdf

## Purpose

GFS is designed to be fault-tolerant. Nodes can fail, but data should
not be lost, nor should the service fail to be accessible.

GFS is optimized for large files. Moreover, it is optimized for
append-only, not random insertion/update. It is also optimized for
large streaming reads, not random access.

Optimized for throughput, not latency.

## Architecture

Single master, many **chunk servers**, which store **chunks**. Files
are divided into **chunks**. Chunks are replicated on several chunk
servers. The master stores file metadata: in particular, it maps names
to series of chunk ids. Clients talk to the master to locate chunks,
but talk to the chunk servers to get the data.

Chunksizes are large. This reduces communication with the master,
since it needs to identify where fewer chunks live. It also reduces
the amount of data the master needs to store; the entire chunk
metadata ideally lives in memory (though updates to metadata are
logged for durability).

The paper notes that the master doesn't need to durably store mappings
of chunk ids to locations on chunk servers. That can be queried on
startup. However, it must store filenames to chunk ids.

## Record Append

When a client asks to add records to a file, the master will assign
one of the chunk servers as the **primary** (if there is not already a
primary). The master responds to the client with the primary and
secondary chunk servers.

The client will push data to all the chunk servers. Next, it will ask
the primary to commit. The primary assigns a txn id to this write,
then instructs the secondaries to commit. It acks the client when all
the secondaries respond.

Note that simultaneous appends can be issued by multiple clients; the
primary serializes the operations via the txn id.

If any secondary fails to write, the operation is considered to have
failed. The client can retry to write. This means that the same record
can be written multiple times in some replicas of the chunk. The
records should therefore represent idempotent transactions (e.g., have
txn ids). Readers can discard duplicates.

**This seems to allow the possibility of submarine writes.**

One problem is to avoid multiple primaries for the same chunk. The
master chooses the primary and stores it. The primary is given a lease
for a limited duration of time; the primary may try to extend this
lease. If it does not, then the lease will eventually expire. The
master must wait for lease expiry before it can re-assign a new
primary. **I suppose it must wait some multiple of the lease time,
because of clock skew??**

## Replica Creation/Re-replication/Rebalancing

The master decides where to place new chunks.

The master maintains heartbeats with the chunk servers. When one goes
down, and when the number of servers with that chunk falls below a
target, it tells the survivors to replicate themselves.

## Stale Replicas

If a chunkserver is down it may miss mutations. Everytime a lease is
given for a chunk, a chunk version number is incremented. Anyone
participating in that lease will receive all acknowledged appends.
Anyone not participating will not have their version incremented.

When the chunkserver comes back up, its stale chunk replicas are
removed by the master from distribution.

**There is the possibility that a client will talk to a stale replica
for a read**; the time it can do so is bounded, because the client
caches the stale chunkserver for only a limited period of time. The
next time it asks for this chunk, it will not receive the stale
server. Note that in an append-only workload this means you only miss
out on some data, not read anything overwritten.

## Summary

This allows for scalable append only write load. There are submarine
writes, but never lost writes. This is because everyone in the lease
must commit an acknowledged write. Anyone who misses the lease is
stale and will be garbage collected.

Reads scale. There is a small probability of reading a stale record.
