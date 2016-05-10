## TODO

* Anycast
* Consistency (Eventually, Relaxed, Sequential)
* FLP
* Gbcast
* Spanner
* Stabilization
    * BGP and TCP? What does this mean?
* Viewstamped Replication
* Virtual Synchrony
    * http://en.wikipedia.org/wiki/Virtual_synchrony

## Del

* <del>Atomic Broadcast</del>
* <del>Bitcoin</del>
* <del>Byzantine Fault Tolerance</del>
    * Practical Byzantine Fault Tolerance by Castro + Liskov
* <del>CAP Theorem</del>
* <del>Clock Sync</del>
* <del>Conflict Free Replicated Data Type</del>
* <del>Deadlock Detection</del>
* <del>DHT (Chord)</del>
* <del>Dynamo</del>
* <del>Failure Detection</del>
* <del>Fault Tolerance</del>
* <del>GFS</del>
* <del>Kafka</del>
* <del>Linearizability</del>
    * Versus *serializable*.
    * Linearizable says that writes to a row should be read by
      subsequent reads (wrt to wall clock time).
    * That's not entirely true with serializable systems. Here,
      transactions need to be processed as if they had been done in
      some possible ordering, even if that isn't compatible with
      wall-clock ordering.
    * There's even a notion of *strict serializability*.
    * Fuck it. Here's a link, but I don't care.
    * http://www.bailis.org/blog/linearizability-versus-serializability/
* <del>Message Queues</del>
    * Also have seen how to do exactly once delivery.
* <del>Multicast</del>
* <del>Mutual Exclusion</del>
* <del>Operational Transform</del>
* <del>Optimistic Concurrency Control</del>
    * <del>Serializable Snapshot Isolation</del>
* <del>Paxos</del>
* <del>Raft</del>
* <del>Replication</del>
    * http://en.wikipedia.org/wiki/Replication_(computing)
* <del>Samza</del>
* <del>Smart Contracts, Ethereum</del>
* <del>Sybil Atttacks</del>
    * This is when you used forged identities to make up more votes.
    * What Byzantine Generals solves (in part).
* <del>Termination Detection</del>
* <del>Three Phase Commit</del>
* <del>Transactions</del>
* <del>Two Generals' Problem</del>
* <del>Two Phase Commit</del>
* <del>Vector Clocks</del>
* <del>Zookeeper</del>
