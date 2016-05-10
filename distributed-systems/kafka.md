Basically talks about Kafka.

The log is going to serialize events into an order. All systems will
process input in this order. This removes a great deal of
non-determinism from messaging. Also: the state of any system can be
summarized by how far it has gotten through the logs.

Two approaches. *Primary-backup* is where a leader gets the writes,
then writes out the state changes to the log, which is then shipped to
the followers. This can be more efficient. In SQL, this would be if
you shipped just the modifications you made to records, not the SQL
statements themselves. In a sense this is *physical*.

The other approach is *state-machine replication*, where writes go to
the log, and all peers perform these *logical* operations in order.

Paxos and Raft are both about maintaining a log, at the end of the
day.

Mentions that logs are more fundamental than tables. I agree.

I guess any distributed system built on top of such a log acheives
eventual consistency, even across multiple rows, though I guess you
can't have availability then. But of course, at any moment there might
be inconsistency.

That is a little facile; if you need to read from one place and write
that data to another, it's not like the two nodes can just execute
these operations independently by processing the log in order; they
need to coordinate.

TODO: Kafka claims to have availability as long as a majority of nodes
are available. This isn't strictly "available". Maybe I should study
more about practical availability, even if it's not 100% available. I
guess one thing people seem to talk about is whether the system is
meant to run across datacenters.

As mentioned, one nice property is that clients can use (logical)
timestamps from the log to ensure that, when they get data from a
client, that this client has processed up to timestamp T.

They also note that clients can process the log at different
rates. They can even crash and continue when they resume. This
decouples the clients from each other; they don't need to process
events in lockstep.

Basically, the log is like a better version of PubSub. It is
implementing a form of atomic broadcast: every message is eventually
delivered to every consumer in the same order.

One thing that is nice about Kafka is all communication between
different parts of the system can be routed through it.

To acheive scale, Kafka partitions the log; there isn't a global
order. The client can control which partition to write to, though, so
they have the ability to partially order some messages. The log is
replicated; single master, but followers for reading. They don't see
this lack of total order as a big problem; since events come from many
many writers, none of whome have their events ordered relative to each
other.

Because of linear ordering of records, there is much opportunity to
batch reads/writes. This is going to allow very high-throughput. They
say that you can saturate disk/network, and deal with logs greatly
exceeding available memory. In short: they say they are able to get
the most out of their hardware this way.

They talk about Samza a bit. I think the major difference between
Samza and Storm is that Samza stores a stream processor's state in a
recoverable fashion. This state can be recovered if the processor
crashes.

Kafka can retain records for a fixed duration, or up to a certain
size. You can also do *log compaction* instead; here you need tuples
with keys, and only the latest tuple for that key is retained.

## Source

https://engineering.linkedin.com/distributed-systems/log-what-every-software-engineer-should-know-about-real-time-datas-unifying
https://aphyr.com/posts/293-jepsen-kafka
