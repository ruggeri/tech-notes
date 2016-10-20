* Puma/Webrick
* App Tier, EC2
* DB: RAM vs disk
    * RDS
* Scale Up
* Load Balancer, NGINX, AWS ELB
* Leader-Follower Replication
* **Log formats**
* **"Back in time" and tokens**
* **Async and Failover**
* Sharding
* Joins don't scale
* Denormalization
* **What is ACID?**
* Sharding breaks ACID
* Two Phase Locking
* **Isolation Levels**
    * Read Uncommited: dirty read
    * Read Committed: non-repeatable read
    * Repeatable Read: Phantoms
        * B-trees?
* **MVCC**
* Two Phase Commit
* Latency across datacenters.
* **Paxos, Raft, Zab**
* Availability is always the problem.
* Idempotency: retrying an interrupted transaction.
    * Can be accomplished with tokens.
* Message Queues, Kafka

**TODO**

* Commutative Operations
* CRDTs
    * They acheive strong eventual consistency.
* Eventual Consistency
* DNS round robin. Route 53
* Failover:
    * Failing an app server, load balancer, follower.
    * Leader Election
* CAP:
    * Cassandra vs HBase
    * Mention DynamoDB as an Amazon service.
    * Eventual consistency makes sense here.
* MongoDB: what is a document store?
* SOA
* Friend/Defriend/Locked Friend race condition
    * We can use timestamps and last-write-wins, but the conflicts
      cannot be resolved in a way that preserves linear evolution of
      changes.
    * Demonstrates a possible situation where a commit is *reversed*.
* Review Database Systems book on distributed locking (chapter 20)?

**Other**

* ElasticSearch
* Docker, Elastic Beanstalk
* CDN, CloudFront
    * Minification and concatenation
    * Cache-Control
    * Redis, ElasticCache
