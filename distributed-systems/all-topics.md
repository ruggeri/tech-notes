* Anycast
* <del>Atomic Broadcast</del>
* <del>Bitcoin</del>
* <del>Byzantine Fault Tolerance</del>
    * Practical Byzantine Fault Tolerance by Castro + Liskov
* CAP Theorem
* <del>Clock Sync</del>
* Conflict Free Replicated Data Type
    * http://en.wikipedia.org/wiki/Conflict-free_replicated_data_type
* Consistency (Eventually, Relaxed, Sequential)
* <del>Deadlock Detection</del>
* <del>DHT (Chord)</del>
* Dynamo
* <del>Failure Detection</del>
* <del>Fault Tolerance</del>
* FLP
* Gbcast
* GFS
* Kafka
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
* Message Queues: exactly once delivery?
* <del>Multicast</del>
* <del>Mutual Exclusion</del>
* Operational Transform
* <del>Optimistic Concurrency Control</del>
    * <del>Serializable Snapshot Isolation</del>
* <del>Paxos</del>
* <del>Raft</del>
* <del>Replication</del>
    * http://en.wikipedia.org/wiki/Replication_(computing)
* Replicated File System
* Samza
* <del>Smart Contracts, Ethereum</del>
* Spanner
* Stabilization
    * BGP and TCP? What does this mean?
* <del>Sybil Atttacks</del>
    * This is when you used forged identities to make up more votes.
* Tao
* <del>Termination Detection</del>
* <del>Three Phase Commit</del>
* <del>Transactions</del>
* <del>Two Generals' Problem</del>
* <del>Two Phase Commit</del>
* <del>Vector Clocks</del>
* Viewstamped Replication
* Virtual Synchrony
    * http://en.wikipedia.org/wiki/Virtual_synchrony
* Zookeeper
