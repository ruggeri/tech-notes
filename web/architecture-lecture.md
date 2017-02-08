**Basic App Structure**

0. App server, Web Server, DB Server
0. Box, EC2
0. Very basic DNS

**Basic App Performance**

0. Rails App logic can be slow.
0. JRuby
0. Webrick vs Puma; multi-threading.
0. DB Perf: RAM vs Disk. Reads vs Writes.
    * Can mention RDS.
0. Scale up is easy; do it first.
    * More cores, more ram, faster disks.

**Scaling Out App Tier**

0. Nginx as a reverse proxy.
    * Mention ELB.
0. Add app machines. They can be cheap for best perf/cost ratio.
0. DB is chokepoint; runs on a fancy machine.
0. Also increases redundancy.
0. Nginx can use various methods to distribute queries across
   machines.

**Scaling Out DB Reads**

0. Scaling App Tier doesn't help DB, which is a likely bottleneck.
0. Leader-Follower. Read load is distributed, write load is not.
0. Read load is most likely much greater than write load.
0. Writes serialized and batched at the leader.
0. This works great for SO which has 2 DB machines per site.

Maybe mention here the possibility of reading stale data from a
follower if we ship logs async (as is typical).

**Distributing Write Load**

0. Every write needs to end up at every machine. So if a multi-leader
   means every write eventually gets processed at every DB replica,
   what is the point?
    * There of course is a point, but we won't talk about that yet.
0. Also, we'll talk about how multi-leader can lead to consistency
   problems *later*.

**Sharding and Denormalization**

0. Split the DB into shards each with 1/Nth the data.
0. For transactions that read/write a single row, this is ideal.
0. Joins are not very scalable because they involve all shards.
    * It sounds like, inside a datacenter, this is not always true.
    * For instance, you can do a distributed join: hash the join key
      and shuffle. This is like a Map-Reduce step.
    * This is what things like Terradata can do.
    * But I think the query optimizer has a really hard job.
    * Also, you're probably more susceptible to the latency of other
      machines.
    * I actually feel kinda week on this.
0. You can denormalize the data, as in the friends example.
0. You can better perf, but this is harder to keep up-to-date.

**Sharding ACID && 2PL**

0. A problem for sharding will be ACID.
    * They don't know ACID. Explain atomicity, durability,
      consistency.
0. Explain isolation by reverting to a non-distributed database. Show
   isolation failure without locking.
0. Mention a lack of commutativity is what causes the problem.
0. Demonstrate 2PL.
0. Explain deadlock.
0. To conclude: 2PL achieves serializability.
    * You can mention MVCC as an alternative.

**Sharding Breaks Isolation && 2PC**

0. Show how sharding breaks isolation.
0. Why wasn't this a problem in leader-follower? Because only one
   sequencer of transactions.
0. Solution is to bring 2PL to distributed, which is 2PC.
0. Do Cat Shelter example.
0. Note availability problem if coordinator dies.
    * I don't think you have to discuss that the truest problem is if
      coordinator + a commit site dies. That would take us toward 3PC
      and that isn't at all needed.

**Availability and Latency**

0. Downsides to 2PC are (1) the need to synchronously communicate with
   every site. This can be slow if data is geo-distributed.
0. Also, latency holding locks is pretty bad for concurrent processing
   of transactions, because this can cause pile-ups and deadlock. That
   is: latency just doesn't make your query a little slow, but also
   adds latency to any *other* query that wants that data, too.
0. (2) availability problem if the coordinator dies.

**Availability and Leader Election; CAP Theorem**

0. So we're talking about failures. Easy to fix for load balancers
   (via DNS round-robin) and app machines (load balancers keep track).
    * Mention AWS Route 53.
0. Let's go back to leader-follower replication.
0. What happens if the leader dies?
0. We need to bring another online. But someone can't just assert they
   want to be the new leader.
0. This is a scenario for Paxos/Raft/Zab.
0. But any solution makes you choose Partition or Availability.
0. Async Replication + Failover means bounded dataloss.

**Multi-Master: Replication + Consistency**

0. Say you have a multi-master setup with replication.
0. Then you can do primary-copy locking, but this means if that site
   is down, you're screwed.
    * There are lots of variants like forcing someone to lock half the
      sites or whatnot.
0. Also, you're going to be susceptible to partition and availability
   problems.
0. But if you keep availability, then you may accept two simultaneous
   conflicting transactions.
0. A simple resolution is to do last-write-wins.
    * This means your system is *eventually consistent*, but there are
      periods of time where it is reporting different things to
      different people at different datacenters.
    * One example is Friend/Defriend/LockFriend. If we're friends, and
      I lock you down at the same time you defriend me in another data
      center, someone's action is lost.

**Cassandra, HBase, MongoDB**

* Cassandra offers:
    * Distributed datastore.
    * NoSQL
    * Auto-sharding. Add more machines, handle more load.
    * Replication: lose machines, this data has been replicated at
      other shards.
    * Available; any replica can accept a write.
    * Eventually Consistent: Last-Writer-Wins.
* HBase is mostly the same except:
    * Consistent. Not always available.
* **TODO**: MongoDB

**Multi-Master: CRDTs**

0. In certain cases, where operations commute, then everything is
   okay.
0. A good example is adding to a set (no removal).
0. In that case, on the healing of the partition, everything is okay
   again.
0. In particular, on each side of the partition, it doesn't look like
   anything was rolled back. Because `T1 then T2` is the same as `T2
   then T1`, the two machines may feel like the transactions were
   ordered differently, but they both 'advance' to the same common
   result.

**Queues + Idempotency For Durability**

0. If we need to make an update to two rows at different shards, how
   do we make sure we effect both changes?
0. We can put the message into a queue, which will not lose the
   message.
0. If interrupted, it may deliver the message more than once.
0. An *idempotent* operation has no effect if repeated.
    * You can achieve this by having unique transaction IDs and
      keeping a list of them.
0. For instance, imagine a counter.
0. Kafka is a common queue software.

Redis, Elasticache
ElasticSearch
Docker
CDN, CloudFront
* Cache-Control headers

databases/principles-of-distributed-database-systems.md
databases/scaling.md
distributed-systems/all-topics.md
distributed-systems/facebook.md
distributed-systems/spark.md
randos/architecture-problems.md
randos/papers/bigtable.pdf
randos/papers/chubby.pdf
randos/papers/dynamo.pdf
randos/papers/f1.pdf
randos/papers/gfs.pdf
randos/papers/mapreduce.pdf
randos/papers/megastore.pdf
randos/papers/percolator.pdf
randos/papers/spanner.pdf

**Bonus Topics**

* Log Formats (as part of Leader-Follower)
* Back-in-time prevention with tokens (as part of async Leader-Follower)
* Isolation Levels (as part of 2PL)
* MVCC (as part of 2PL)

**Maybe?**

0. Claim HBase is good for reads.
    * Because of "single-write master".
    * I think they mean primary-copy with sync write lock on the other
      replicas.
        * This is of course somewhat slow.
    * Won't do secondary indices for you.
0. Cassandra
    * Cassandra has fast single-row read performance, so long as
      eventually consistency is okay.
    * Otherwise, it needs to do a quorum read, which is slower.
    * Writes are much faster than reads (because LSMT)
        * I think "reads are slow" is mostly meant relative to write
          throughput.
        * LSMT does slow down reads a bit because you have to look at
          several levels
        * I think this assumes also that random reads hit disk.
    * Does do secondary indices. But appears to be more limited than
      MongoDB. Can only index a single column, and by equality.
0. MongoDB
    * Strong consistency because primary-copy syncronous replication.
    * Expressive query language
    * Secondary indices. I think MongoDB is suppose to have the best
      support for secondary indices.
        * I think that's what lets it have nice flexible querying.
    * A lot of people do seem to say that MongoDB is most nice for
      being able to flexibily develop, and that scalability is less
      the primary reason to use MongoDB.
    * Data split into replica sets. There's a master in each
      set. Sounds like primary copy replication to me.
        * Again, makes writes somewhat slow.
