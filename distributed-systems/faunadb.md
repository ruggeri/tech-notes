FaunaDB is a distributed DB based on the Calvin research project. It
wants to offer strict serializability (i.e., linearizability, i.e.,
"externally consistent" is the Google Spanner word).

It wants to be a Spanner competitor, but with no clocks. It is a
Cockroach competitor.

The DB is partitioned, with replicas spread across the globe. There is
a distributed transaction log. For efficiency, transactions are
batched, and then the system agrees on a next batch of transactions.

A tx is submitted to a coordinator. The coordinator chooses a
timestamp, and does all the reads it needs to. It keeps track of all
the read data values. It also keeps track of all the "writes." None of
these are applied yet.

What is submitted to the distributed log is the record of all the
reads/writes performed by the coordinator. Presumably, if any values
are changed, then the transaction is rejected.

## Thoughts

This feels like it will limit parallelization of updates. The updates
must be performed in exactly the specified order in the transaction
log. (*Wrong*, each transaction at most inserts a new timestamped
version of a record).

People note that strict serializability is going to require either (1)
causality tokens (which are hard to get right for application
developers) or (2) hit the log, which is going to cause latency.

This is presumably an advantage of the TrueTime approach of Spanner.

People note that FaunaDB transactions are effectively non-interactive,
more like stored procedures. But the FaunaDB people say this isn't a
big inconvenience for developers.

**TODO**: This is not a very full comparison. I haven't had a lot of
time to think about pros/cons.

## Source

https://fauna.com/blog/consistency-without-clocks-faunadb-transaction-protocol

Good discussion:

https://news.ycombinator.com/item?id=18257128
