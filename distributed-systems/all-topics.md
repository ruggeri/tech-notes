## TODO

* Anycast
* Consistency (Eventually, Relaxed, Sequential, Strong, Strong Eventual)
* FLP
* Gbcast
    * Apparently almost equivalent to Paxos.
* Spanner
* Stabilization
    * BGP and TCP? What does this mean?
* Viewstamped Replication
    * Apparently almost equivalent to Paxos.
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
    * Serializable doesn't have to do that. It can reorder
      transactions however they want. All your reads can be from the
      very beginning of the system. All your writes can be deferred to
      a much later time.
    * So then there's *linearizability*. This says that once a write
      completes, any read that starts afterward should reflect this
      write.
        * Linearizability is really about a single item (not
          transactions).
        * Memory reads/writes in a multi-core CPU are typically not
          linearizable.
        * It sounds like this word comes from parallel programming
          people (especially Herlihy)
    * There's even a notion of *strict serializability*.
        * This is just linearizability but applied to the entire
          transaction.
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
