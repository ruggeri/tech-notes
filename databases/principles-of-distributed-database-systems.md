## Ch1: Introduction

* DDBMS: Multiple logically interlated databases over a network.
* Differences between DDBMS and parallel DBMS is vague. Parallel DBMS
  in a shared-nothing architecture is very similar to DDBMS. Main
  difference is assumption of homogeneity. Parallel DBMS is also
  covered in this book!
* Promises:
    * Transparency; should act like one database.
        * Network: shouldn't have to think of network.
        * Replication: Shouldn't have to think of keeping replicas
          up-to-date, or which one to use.
        * Fragmentation: shouldn't have to do the sharding
          ourselves. Queries should be broken up to shards for us.
    * Increased reliability.
        * Should be resilient to failures.
        * Maybe failures take down part of the system, but the rest
          can operate.
    * Performance
        * More sites means pooling of CPU and IO power.
        * Closer sites means better latency.
    * Easier scaling.
        * Should just be able to add new sites.
* Complications:
    * Replication: how to choose where to store, how to receive, how
      to keep all copies up to date.
    * How to deal with failures.
    * How to keep things synchronized across sites when there may be
      high latency between them. In particular, how not to degrade
      performance too much.
* Design Considerations:
    * Distributed Query Processing.
    * Distributed Concurrency Control.
    * Distributed Deadlock Management.
    * Reliability.
    * Replication.

## Ch2: Background

* Types of network:
    * Bus: everyone shares one channel. Senders need to listen to hear
      if anyone else tries to send simultaneously. Choose a random
      backoff.
        * This approach is called CSMA/CD: carrier sense multiple
          access with collision detection.
        * Actually, it looks like we don't really need this now sense
          everything is connected via switches.
        * Hubs create shared collision domains, but no one uses those
          anymore. Their only advantage was price over switching
          technology.
    * Star: people speak directly to a central node that forwards the
      message to the recipient.
    * Ring: token passes around.
    * Mesh: everyone has a point-to-point interconnection.
* Unicast networks have packets sent to recipient via
  switching/routing.
* Broadcast networks communicate over channel utilized by all nodes.
    * Some networks support multicasting where message is sent to a
      subset of nodes.
* Broadcast is mostly radio or satellite. Mentions HughesNet as an
  example!
* Measures:
    * Bandwidth: bits per second.
    * Data transfer rate: after you account for overhead of
      encoding/decoding, error correction, appending headers/trailers,
      etc. Basically: the usable bandwidth.
* Circuit vs packet switching
    * Circuit exists for entire connection. Presumably a path is
      dedicated for the line. In electro-mechanical systems, I assume
      this is done by an operator patching you in. Indeed, operators
      were the original switches!
    * Packet switching is where each packet is addressed and routed
      independently. They may even take different routes or arrive
      out-of-order.
    * Packet switching is especially useful because we don't need to
      dedicate bandwidth to a circuit; we can better utilize overall
      bandwidth. This is important because traffic is bursty: a
      circuit would need to be provisioned at maximum capacity, even
      if it typically uses much less capacity than this.
    * Circuit switching can be better when packet switching introduces
      to much extra delay or delay variance. This affects multimedia
      applications.
* Talks about protocol stack:
    * Application message.
    * Then TCP/UDP layer.
        * TCP: requires setup. In-order, reliable. Flow-control to not
          overwhelm receiver with data. Congestion control to backoff
          when network is saturated.
        * UDP is connectionless. No such guarantees; preferred if
          application wants to choose what guarantees it needs.
    * Then IP layer.
    * Then whatever is needed for the physical network.

## Ch3: Distributed Database Design

* Fragmentation:
    * Pro: increases intraquery concurrency.
    * Con: if data is needed together (like for a join), requires
      communication from distant sites.
    * Con: Enforcement of constraints across sites is harder.
* Allocation of fragments
    * Where to place them.
    * Are there replicas?
    * Is there a primary site?
* Extremes:
    * Partitioned DB has no replication but is fragmented.
    * A fully replicated DB has no fragmentation and is replicated at
      multiple sites.
    * Partially replicated DB is where fragments are replicated, but
      not to everywhere.
* Horizontal Fragmentation:
    * They talk about primary vs derived horizontal fragmentation.
    * Primary is where you look at a row and where to place it depends
      on data in that row (e.g., hash of row id).
    * Derived is when you place a row based on where you placed a
      *parent* row.
        * For instance, maybe you place a Factory row based on
          location.
        * Then Worker rows might be placed according to where the
          factory is located.
* Vertical Fragmentation: breaks up tables.
* This chapter felt dumb. I think we often just break up our DB by
  picking a datacenter closest to a client.

## Ch4: Database Integration

* This is about how to integrate disparate databases that weren't
  originally designed to work together.
* Most of it is about using heuristics to try to find matchings of
  columns in different schemas.
* Sounds ridiculous.
* Useless chapter.

## Ch5: Data and Access Control

* View management
    * Could be expensive to query virtual views.
    * Talk about materialized views.
    * Could be expensive to maintain. Immediate or deferred
      maintenance.
    * If deferred, could either do it lazily (at read time), or
      periodically (at predefined intervals).
    * Lazy means you always read correct data, but is more
      work. Periodically introduces stale views.
    * Also mentions incremental update of views. This is a major
      performance win.
    * If your view doesn't do anything too crazy, you can often update
      it with just the old version of the view and the "difference
      set"; rows that were added/removed/updated.
    * If your view is more complicated, you may also need to use
      unchanged data in the base views. I expect that's the case for
      joins.
    * If you need to requery the base relation across sites, that's
      going to be expensive. So different kinds of views may be more
      easily distributed.
* Data Security
    * Don't care about this.
* Semantic Integrity Control
    * Constraints are either checked after modification but before
      commit, or *pretested*, if it is possible to prove whether a
      constraint will be violated.
    * Basically nothing of interest was said. Just that you probably
      need to do joins to test constraints.

## Ch6: Query Processing

* To pick a plan, we want to optimize either total cost, or response
  time. It might not be the same if more work can be even more
  parallelized, resulting in lower respose time for a query, but
  greater work.
* Traditionally CPU and IO cost were considered, but we must add
  communication now.
    * Traditionally we assume slow network, and ignore CPU and IO
      cost.
    * But if network bandwidth is approximately equal to disk
      bandwidth, then we should weight all three factors.
    * Even with high network bandwidth, transmission involves wrapping
      in a bunch of protocols which can be slow.
    * IB, which can allow remote direct memory access (DMA), will
      further change planning.
* Complexity:
    * Typically, one relation operations can be done in `O(n)` time
      since it's a table scan.
    * Two relation operations that require pairing by equality of some
      attributes (e.g., join, deduplication) is typical `O(n log n)`
      if you use a merge sorting approach.
        * If you have enough memory, you can do it in `O(n)` with hash
          approach.
* Lot of ways to plan a query, but an exhaustive search of (almost)
  all plans is typical because planning is low cost next to execution.
    * Other approaches are randomized, such as iterative improvement.
* Plans can be made statically (at query compilation time, using
  estimates from the DB), or dynamically.
    * Dynamic planning means you could even optimize as you execute
      the query.
    * But probably easiest just to do at start.
* Planning can be decentralized. That might make sense if doing things
  dynamically, so that the site involved with an operation can decide
  how best to run it.
* In wide area networks, can just consider communication cost.
    * So you pick a strategy overall that minimizes communication.
    * Then you let each site do whatever is most efficient using
      typical centralized algorithms.
* Suggests a series of transformations:
    * Query is decomposed into operations on global relations.
    * But then we pull in what fragments this will involve.
    * Then we try to optimize this to do the least communication.
    * Then this can be executed.

## Ch7: Query Decomposition and Data Localization

* This is going to go more in depth about query processing.
* Query decomposition is just whatever techniques are typical for
  centralized DBMS.
* "Localization" just means translating from global relations involved
  to fragments of relations. This is the first actual plan.
    * In particular, if records share sharding key, not every fragment
      needs to send data to every other fragment.
    * Or if fragment contradicts a selection, then we can just drop
      this one.
* They just talk about a number of simple rules and heuristics for
  transforming the global query to a query on fragments.
* The goal is to produce a decent query, but one which will still
  needs guidance from statistics, which have not yet been used.
* Basically we're just eliminating redundant or unnecessary parts of
  the query based on algebraic laws. We're not really developing a
  "plan" for it.

## Ch8: Optimization of Distributed Queries

* Greedy: try to step-by-step build up the best plan.
* Dynamic programming: tries to break down planning into smaller
  subproblems it solves.
* But dynamic approaches run into trouble as number of relations in
  the query grows. So randomized strategies are often used.
    * It sounds like simulated annealing is actually used! Start with
      a random query plan, then make iterative improvements. Restart
      several times.
* In 1980, cost of one page worth of network communication to disk IO
  was ~20:1 in a WAN. For early ethernet (10Mbps), this was 1:1.6. So
  this is saying that for geo distributed applications, just think of
  communication, but inside a datacenter you need to balance. Of
  course, it's harder to balance IO and CPU too.
    * If we want to measure response time, rather than total load, we
      need to factor in parllelization too.
* Most of what we do is figuring out the size of intermediate joins.
* Select:
    * To figure the cardinality of a select, try to estimate %age of
      rows kept.
    * For an equality `A=value`, you can say it's `1/card(A)`, where
      `card(A)` is the number of values for that attribute.
    * For a `A < value`, you can use
      `(value-min(A))/(max(A)-min(A))`. Likewise for `A > value`.
    * Obviously these assumptions assume uniform distribution, but
      what else could you do...
* Joins: Look at DB Systems chapter 16 notes.
* Instead of assuming uniformity of values, we can use
  histograms. That could be useful for things like equality or range
  predicates.
* Typical approaches are greedy (INGRES) and dynamic programming
  (System R). See Chapter 16 notes of DB Systems book.
* For distributed queries, they recommend sending the smaller join
  operand.
    * Or the *semijoin* way is to send the keys of operand R, compute
      what is kept of operand S, and send that part of S to R. This
      works well especially if there is a high selection factor.
    * But semijoin is less useful if the network is fast, because of
      increased processing time.
* Bushy trees vs linear trees
    * We argued that bushy trees were not that useful for centralized
      query planning.
    * But they express parallelism for distributed queries.
* Their presentation of the query decomposition algorithms was really
  confusing to me. I think that the approaches they presented will
  still only consider left-deep trees even in a distributed system.
    * That doesn't use ability to do joins in parallel.
    * OTOH, if every fragment is involved in performing a join, then
      all the machines are already busy, so parallelization isn't that
      useful.
* I don't know. I'm pretty disatistfied with this section...

## Ch9: Multidatabase Query Processing

* Skipped this. Looks uninteresting.

## Ch10: Intro to TX Management.

* By *DB consistency* they mean no constraints are violated. By *TX
  consistency* they mean isolation.
    * *One-copy equivalence* means that all replicas have the same
      value when a TX ends.
* This had basically no information for me. All basic stuff.

## Ch11: Distributed Concurrency Control

* Pessimistic vs Optimistic. Early or late synchronization.
* There are pessimistic versions of locking and ordering algorithms,
  which are most common.
* Locking can be centralized (one site), primary copy, or distributed.
* TO can be basic, multiversion, or conservative (I don't know that
  last one...).
* They make a note about 2PL vs strict 2PL. Strictness is where you
  release all locks only after commit/abort. 2PL allows you to release
  locks one-by-one slowly.
    * However, if you do that, you may be allowing some people to read
      data before it's been committed.
    * If you really never lock anything again, that should be okay.
    * Except if you abort the transaction! Then you'd have to do a
      cascading abort of anyone who read any read uncommited data.
    * Also, how can you be sure you won't modify that data again? I
      guess you could be optimistic and abort if you did return to
      it...
* As said, you can do this centralized, or distributed, where the
  locking is managed by the site. We're not talking about replication
  in this chapter, so we're keeping things simple :-)
* For distributed TO, to generate timestamps, we can use a global
  counter, but that'll be hard to maintain.
    * Well, it would specify that there is one transaction managing
      site.
* Instead, just use a counter at each site, and append the site's ID
  to resolve conflicts.
    * A lot of restarts can occur if one site doesn't get many
      queries, so its counter falls way behind those of other sites.
    * So transactions that initiate at this site would be too old to
      successfully get any reads/writes done.
    * In that case, maybe a true clock timestamp would be better. I
      don't see why timestamp,counter,site would be a bad choice.
    * Well, I guess if clocks drift. So maybe every once in a while
      everyone exchanges current times, and they set their clock to
      one after this. This keeps everyone from drifting too far, I
      suppose.
* *Conservative* TO tries to have each site execute operations in
  increasing timestamp ordering to prevent restarts. They mention some
  ways to do this, but they sound dumb.
* They talk about *optimistic* concurrency control, which just checks
  timestamps at the end of the transaction.
    * They note that you don't have to actually record timestamps if
      you keep read and write sets for recent transactions.
* Wait-For Graph (WFG) is used locally to detect deadlock. But
  ensuring no cycles in local graphs isn't enough. You need to check
  in the *union* of all graphs.
* You could try to prevent deadlocks from happening in the first
  place. But that won't be possible unless transactions predeclare all
  variables they'll use, which is not feasible.
* Ways to avoid deadlocks:
    * Always lock things in the same order. But that isn't so easy!
    * Also you can give TXs timestamps. Wait-Die says that older
      transactions wait for younger transactions to finish, but
      younger transactions just die and try again.
    * Wound-Wait says that older transactions wound younger ones,
      while younger ones have to wait.
    * Either scheme ensures that deadlocks cannot occur.
* But cycle detection in the GWFG (global wait-for graph) is most
  common.
    * If a cycle detected, who to abort? To find the minimum cost to
      break a cycle is NP complete.
    * Quantities that matter: how much invested, how much left to go.
* One simple approach would be to have the sites transmit their LWFG
  to a central deadlock detector. But this could have high
  communication cost, and of course has a SPOF.
    * But then you just do a simple algo in the GWFG.
    * Would set an interval for updating the GWFG. Don't have to do
      this so very often, because deadlock is fairly rare.
    * This has been proposed for distributed INGRES, so it's not
      totally naive.
* Edge chasing says that if you notice you're waiting on an external
  resource and you see it's taking a while, send a message to that
  site. The message sent from S1 to S2 consists of `(S1, S2)`.
    * If they aren't waiting on anyone else, then just chill.
    * Otherwise, if they wait on some other site, append that to the
      list of sites.
    * Repeat. Eventually, your messages stop getting pushed forward or
      they come back to you, and you can see the cycle.
    * Okay. But then you have a problem where the cycle is potentially
      found by many people! The solution is to give the transactions a
      timestamp and only the youngest transaction participating in the
      deadlock.
    * This is from Chandy-Misra-Haas. This is from wikipedia, not the
      book.
    * Not sure I've described this really correctly, but it's the
      gist. I could look it up in my distributed algos book.
* The book describes a slightly different algorithm by Obermack. But
  they explain it poorly.
* Relaxing Concurrency Control
    * Garcia-Molina suggests classes of transactions that are
      compatible and can interleave however they like.
    * Lynch suggests certain "breakpoints" in a transaction where
      other transactions are allowed to interleave.
    * Agrawal formalized these approaches under *semantic relative
      atomicity*.
* They talk about nested transactions, but I don't care.

## Ch 12: Distributed DBMS Reliability

* *Failure* of a system is deviation from the specified behavior of
  that system.
* They say that failures are the result of *errors*: a component is in
  error if it is in an erroneous state. The cause of errors is a
  *fault*.
* Permanent faults, or *hard faults*, are irreversible changes in a
  component's behavior. *Soft faults* are either *transient* or
  *intermittent*.
* *Reliability* is the probability that the system does not experience
  any failure during a period of time. *Availability* is the
  probability that the system is operational at a given point in time.
    * For instance, a number of failures may have happened in the
      past, but if the system is currently up then it is *available*.
* Four types of failures:
    * Transaction failures (aborts).
        * They mean deadlocks, basically. User aborts are not
          considered failures.
    * Site failures.
        * Anything in main memory is lost, anything on secondary
          storage is still there.
    * Media failures.
        * Typical solution is to have multiple disks or backups.
        * Typically treated as a local problem.
    * Communication line failures.
        * Errors in messages, improperly ordered, or lost.
        * We focus on lost messages. The network should handle errors
          and ordering.
        * *Network partitioning* is when links fail and two halves of
          the sites lose connectivity.
* Ability to operate when sites are down is a potential strength, but
  adds complexity.
* They talk (a lot) about redo and undo logging and
  checkpointing. They have nothing to add over centralized DBs.
* Mention media failure. Just say to keep backups. Nothing interesting
  to report here.
* Start talking about atomic commitment and 2PC.
    * Reasons a site may abort a transaction include constraint
      violations, but also a TX can be killed for deadlock.
* 2PC:
    * Coordinator sends TX and tells sites to prepare.
    * Sites log their vote, and respond to the coordinator.
        * If vote is to abort, can forget the TX. Otherwise need to
          keep it in case of failure.
        * Because you're promising to commit even if you fail at this
          point.
    * Coordinator collects votes, then tells everyone what to do.
    * They then COMMIT or ABORT as the coordinator says.
    * They ack, and the coordinator can consider the TX complete.
* A variant is distributed 2PC. Here the sites communicate their
  decision to everyone else. Then there is no need for the coordinator
  to tell their decision. More messages, but fewer rounds.
    * Of course, in this case the coordinator has to give a list of
      all participants to the sites.
* Presumed abort and presumed commit are uninteresting optimizations.
* Recovery from failures:
    * When coordinator timesout waiting for a decision from a site, it
      can just unilaterally abort.
    * When the coordinator doesn't receive an ACK of its global commit
      or abort command, it just continues sending these until it gets
      acknowledgement.
        * It can't forget or change its vote at this point.
    * If a site times out waiting for the coordinator to send it a
      global commit or abort command, it can't do anything. It voted
      to commit, so it cannot change its vote now and abort. But it
      can't commit either.
    * In this case, it has to wait until it hears from the coordinator
      or another site.
* Let's say in 2PC that peers can talk:
    * If anyone didn't vote yet, they can tell the others they vote to
      abort.
    * If anyone was told to abort or to commit, they can tell the
      others they need to do that.
    * If everyone has voted to commit we could choose to commit.
    * Except if a site is also offline! Then we can't do anything!
* 2PC is fails if the coordinator plus one participant fail or are
  partitioned away.
* 3PC improves this. If the coordinator and any number of participants
  fail, we can still decide what to do.
    * If everyone has a prepare-to-commit message, commit.
        * The other machines might have!
    * If no one does, abort. The other machines might have!
    * If at least one person has a prepare-to-commit, but not
      everyone, you could do either. Doesn't matter what you default
      to.
* But the problem is with partitions, of course. If the coordinator
  fails and just one node is partitioned away, what do you do?
    * Everyone on one side can get a prepare-to-commit, but no one on
      the other. Then they do opposite things.
* To avoid a situation where both halves of a partition you can try to
  do a quorum like thing.
    * Pick `V_A` and `V_C` for abort and commit. Must have
      `V_A+V_C>V`.
    * It would be natural to pick `V_A=V_C=(V+1)/2`.
    * The description of this algorithm in the book is woeful.
* Of course, in order to handle partitions, it must be possible to
  block. This is a result of Skeen and Stonebraker.
* Book mentions possibility of byzantine errors, but these are not
  pursued.
* To deal with cascading failures and tolerate `<n/2` failures, you
  can do E3PC or Paxos.
    * Can't find any pros/cons except that Paxos dominates mindshare.
    * Btw, Oki and Liskov's viewstamped replication paper is very
      similar to Paxos. This was published 1988, and Paxos was first
      submitted in 1989.
    * Apparently gbcast by Ken Birman is also equivalent to Paxos.
* **NB**: Note that if you're doing distributed commit across
  datacenters, you're talking about maybe 100ms ping between
  centers. So three phases is really actually quite a lot!

**Sources**

* Notes on Data Base Operating Systems
    * By Gray.
    * Section 5.8.3.3 describes 2PC. Appears to be first cited
      description of 2PC.
    * http://research.microsoft.com/en-us/um/people/gray/papers/DBOS.pdf
* Skeen describes a 3PC protocol in his disertation I think.
    * Crash Recovery in a Distributed Database Management System.
* A Quorum-Based Commit Protocol (1982)
    * Skeen wriets up a quorum version of 3PC.
    * https://ecommons.cornell.edu/bitstream/handle/1813/6323/82-483.pdf?sequence=1
* Skeen and Stonebraker formalized results on existence/non-existince
  of non-blocking protocols in 1983.
* Increasing the Resilience of Distributed and Replicated Database
  Systems
    * Paper introduces E3PC.
    * They say that Skeen's version had problems if quorum was lost
      and then sites reconnected.
    * I think their's is the *real* extension to 3PC.
    * http://webee.technion.ac.il/~idish/Abstracts/jcss.html

## Ch13: Data Replication

* If all replicas are brought to consistency by end of transaction, we
  say that *strong consistency* is enforced. Other more relaxed
  approaches are called *weak consistency*.
* Updates can either be performed at a *primary copy*, or updates are
  *distributed* if they initiate at other copies.
* Update propogation from a site at which an update starts can be
  either *eager* or *lazy*. Eager seems to imply strong
  consistency. Lazy allows batching.
* Strong consistency can be enforced by 2PC. *Eventual consistency*,
  where replicas converge if given time, is another approach.
* *Epsilon serializability* allows transactions to read stale data
  from replicas where an update hasn't been replicated to yet. But
  epsilon serializability would not allow reversal of transactions.
* Eager updates will result in mutual consistency. This requires
  writing to all replicas, but you can read from any one replica and
  get the result.
    * They mention that *deferred* eager update means you batch all
      writes after performed locally, then send this to everyone else
      as part of the start of the 2PC prepare message.
* This means you can never read stale data! And you never do a remote
  read.
* The main problem is you'll have to use 2PC, which is slow, and you
  won't have availability in the face of a failure of a site.
* Lazy update propogation means that writes are faster and more
  available, but that you don't have mutual consistency of the
  replicas.
* Primary site can be good because at least one node will be
  up-to-date.
* If a write can happen at any replica, then this is good for latency
  and availability, but now you have a potential of concurrent writes
  to replicas, which will need to be merged.
* First they mention single-master eager replication.
    * All transactions that write must go to the master.
    * The master gets locks.
    * It forwards the writes to the slaves. They must process these in
      the same order. But that is trivial. Just timestamp the
      transactions in the order they start. Slaves apply in this
      order.
    * You can run read-only transactions at slaves.
    * They also mention that you can maybe reduce load on the master
      site if you just make the master site a lock manager.
        * People will go here for read and write locks.
        * But then they can read wherever, and write to everyone.
        * This is an uninteresting alternative. But the idea (I think)
          is that the master doesn't need to even do the reads or
          propagate the writes if that is built into the TM that just
          makes sure to get the writes from the master.
* They do discuss primary copy eager replication. They suggest 2PL to
  ensure serializability.
    * Here you just need one read lock on any copy, but to write you
      need to lock all writeable copies.
        * In the primary case, you have to start at the primary copy.
        * In the distributed case, you can start at any copy.
        * In either case, you need to forward the write eagerly to all
          replicas.
        * They'll have to ensure operations are performed in a
          consistent order at all replicas.
    * Partitioning clearly is better than single-master. Not clear
      that update-anywhere is really that helpful.
        * Easier to serialize updates to the replicas if they're
          coming from the same master.
        * Could check GFS for why they chose primary copy.
        * But I guess you'll need a service to keep track of who is
          the primary.
* They mention centralized lazy replication.
    * This is just master-slave replication.
    * You will be able to read stale data.
        * There are actually some means to achieve session
          serializability.
    * With primary-copy lazy replication, stale data can lead to
      constraint violations.
        * Say you're at a site where you have a primary for X and a
          secondary for Y.
        * Say that if X and Y are both true, you can set one to
          true. But if one is false, you can't change anything.
        * X and Y start true. At site 1, I read X and Y, and I update
          X (lazily).
        * At site 2, I read X and Y, and see that they are both
          true. I set Y (lazily).
        * In time the updates can be applied at the remote sites, but
          we converge to a state where both X and Y are set to false,
          which is not intended!
        * This happens even though we acquire a read lock on the
          remote data and a write lock on the local data.
        * The problem, I think, is basically write skew!
    * This shows that even though you didn't try to edit anything
      simultaneously or lost any updates, you have entered an
      inconsistent state.
    * **Weird**. I don't think this is what they mean, but this is the
      example from the paper they cite...
        * https://pdfs.semanticscholar.org/ed07/98d0a8610c328d87058938630051f92ec9ee.pdf
* They consider a weird proposal: reads at a site, but writes are
  forwarded to where the primary is.
    * **How is this different from before?**
* Of course, lazy distributed protocols are the hardest.
    * Writes can come in anywhere.
    * The problem of course is reconciliation.
    * Last writer wins is common. But if clocks aren't well
      synchronized, then this can give arbitrary bias.
    * Note that you won't just lose things or lose serializability;
      you'll lack even snapshot isolation.

**Notes From Database Replication: A Tutorial**

* Fault-tolerance and high-availability is acheived through
  replication.
* But also lower latency (if in datacenter closer to you) and higher
  throughput (can send queries to more machines).
* Also, think about mobile clients. They can be thought of as
  replicas, too. They are frequently disconnected from the network.
* In particular, consistency is in tension with performance.
* They only talk about read-one-write-all(-available). They want you
  to go elsewhere for quorum systems. ROWAA means you read and write
  at one location, but then send your writes to the appropriate remote
  location(s), too.
* They identify two primary dimensions: is there a primary, or can you
  write anywhere. Is update propagation eager, or is it lazy.
* They'll assume strict 2PL through most of the lecture. But they
  mention other techniques are possible.
* Eager with Primary
    * Write transactions go to primary.
    * Read transactions go to secondaries.
    * A read transaction might deadlock with a write transaction at a
      secondary.
    * Then the read transaction should be killed.
    * If write transactions deadlock, the primary can decide what to
      do.
    * This gives you global serializability, because all locks are
      acquired in the same order at all repliacs.
    * We say this is not "transparent" because the client needs to
      know who the primary is.
    * Sometimes secondaries will offer forwarding to a primary, but
      how are they supposed to know whether a TX is read only or
      write? They can't know at the beginning of the TX.
    * It sounds like the real problem is when you don't know whether
      your TX will be read-only or whether it will update anything...
    * But generally simple and gives global serializability. Problem
      is that it's slow.
* Eager Update Anywhere
    * With Eager Primary, you don't have problems with global
      deadlock. Local read transactions can get in the way, but
      secondaries can just kill these.
    * You still use distributed 2PL and 2PC.
    * But now you might kill a remote transaction if a node detects
      global deadlock.
    * Distributed deadlock algorithms can be tricky. If you set a
      timeout on a TX as too low, you kill TXs unnecessarily. Else,
      you might leave parts of the system unavailable until deadlock
      is resolved.
        * Mention that Gray indicated this was one of the reasons for
          low throughput.
    * For this scheme, the local site managing the TX waits for each
      remote lock to be ACKed.
        * In the primary example, they don't wait for lock ACKs.
        * They say they could change this, and will explain why later.
* Lazy Primary Copy
    * Primary accepts writes. At commit, will then multicast the
      changes. They assume reliable, ordered multicast.
    * The secondaries apply the changes in the multicast order.
        * BUT they'll probably want to apply changes in parallel, so
          they should multi-thread, but should use locking.
        * I guess they can write in any order, so long as they lock in
          the proper order.
        * They also need to lock so that local read transactions will
          not see intermediate results. Read transactions can of
          course deadlock with updates, but then we just kill the
          local read transactions.
        * Weird. You obviously need to lock in receiving order (the
          order primary applied). But if you just run blindly in
          parallel on each TX sent, a later TX can grab a lock that an
          earlier TX needs.
        * So I guess the only thing that can be parallelized is the
          actualy *writes*.
        * That probably could be really significant though.
    * Will still give serializability, but read TXs at secondaries may
      see "old" data. You won't have *strong serializability* where
      later TXs see effects of earlier transactions.
    * A problem can occur if a primary *fails* and someone takes over
      before the primary informs the others. The new primary won't
      know about that last commit, so it can accept other conflicting
      transactions!
        * If the simplest thing to do is reverse the TX at the old
          master when it recovers, this sacrificies durability.
        * Otherwise, not clear how to resolve. Will need some
          strategy.
        * Indicates that at failover, need to talk to the secondaries!
    * Note that this cannot happen in the eager modes!
    * Hopefully we are going faster because there is eager
      communication. But the user may still be far away from the
      primary.
    * For that reason, we can try to partition the dataset, and make
      the primary for parts of the dataset close to those who use it.
    * But it can be challenging to know how to locate this data.
* Lazy Update Anywhere
    * Accept writes at any site. These are then lazily propagated to
      other sites.
    * Best performance. But now there can be conflicts between
      transactions! We can't get global serializability.
        * But we probably at least want *eventual consistency*.
        * That means that eventually different sites will agree on
          values.
        * One way to acheive this is to let writes from TXs with newer
          timestamps overwrite those with older timestamps, and ignore
          writes from an older TX if we've already made an update from
          a TX with a newer timestamp.
        * This is called the Thomas Write Rule
* Correctness Criteria
    * Strong Consistency means all replicas have same value at the end
      of a commit.
        * In particular, it means that all conflicting transactions
          are applied in the same order.
        * Typically achieved by eager and 2PC.
        * Also they mention that so long as the master knows that the
          replicas have the writeset and will all commit it in the
          same order, we don't need to do 2PC.
        * With ordered, reliable multicast, just need to broadcast t
          writeset from the primary.
        * Ordering ensures writesets can be applied all in the same
          order. Reliability means everyone gets it.
        * They mention that reliable ordered multicast may typically
          have a stop failure model, not crash-recovery.
        * You need to have some kind of eagerness to achieve strong
          consistency. Otherwise, no one might have been sent the
          writeset from the primary!
    * They talk about "atomicity" by which they mean does a confirmed
      transaction get lost. I would have called that durability...
        * They mention that eager schemes provide both atomicity and
          strong consistency.
        * They mention that at atomicity and strong consistency go
          hand-in-hand and they don't know any scheme that achieves
          one and not the other.
    * Weak Consistency
        * Stale data may be observed, or even temporary
          inconsistencies.
        * Staleness is characteristic of lazy primary replication.
    * Eventual Consistency
        * Typical of lazy update anywhere protocols.
        * Transactions may conflict, resulting in transactions being
          aborted after ACKed, or unserializable behavior.
        * But if all replicas eventually reach the same result, we
          have eventual consistency.
    * Serializability
        * I guess you could have strong consistency but not
          serializability?
        * Snapshot isolation is also popular.
        * 1-copy equivalence means that even though there are multiple
          copies, they act as if they are one.
    * Linearizability and sequential consistency.
        * They don't define these!
        * But linearizability is often synonymous with strict
          serializability, which means that transactions that start
          later see the results of transactions that ended before.
        * *Linearizability* in concurrent computing means an operation
          is indivisible and you can't see intermediate results. But
          also, things should not be reordered in a way not compatible
          with time.
    * Session Consistency
        * This means that within a session, we look like we're going
          forward. This can be achieved even in lazy primary
          replication if you have a transaction identifier that is
          monotonically increasing for the session.
        * Of course, this is for free with 2PC.
* Reducing # of Messages
    * With the eager primary 2PC way, they had us making a request for
      each lock, one-by-one.
    * An alternative is to do everything locally, then send the
      writeset at the end when the user commits. Then do 2PC to verify
      the replicas did it.
        * This reduces message rounds, but means the work of forwarded
          writes is not performed in parallel with the primary's work.
        * I don't know that a write to the DB is actually that slow,
          but it does involve at least a disk operation.
        * IO can be much slower than messaging inside a datacenter.
        * But on a WAN, we may want to limit message rounds because
          they're high latency.
    * A second alternative is to send the writeset only when the user
      sends commit, and then have the secondaries acquire locks. When
      all the secondaries ack that they have locks, the primary
      commits, and then ACKs to the client. No 2PC is needed.
        * There is only one message round.
        * The secondaries commit as soon as they acquire the lock.
        * This is more like the reliable multicast way of doing things.
* Statement vs Object Replication
    * You can forward SQL statements, or just the changed rows.
    * Changed rows are smaller, typically. Faster, because don't need
      to execute.
    * Even for very simple SQL queries, you won't have to parse.
    * Also: SQL can contain non-deterministic user defined functions
      (like current time). So may be able to achieve determinism
      easier with object replication.
* Optimistic Concurrency Control
    * You do your work in a private workspace, and verify before
      commiting that you didn't read any data that anyone else wrote
      in the meantime.
    * Multiversion allows you to read data other people wrote, so long
      as you can "slot in" before them.
    * MVCC typically is used to achieve *snapshot isolation*.
    * You can use OCC and MVCC with primary replication, and it
      doesn't change anything, just the concurrency control method
      used at the primary.
    * Offers new possibilities for update anywhere.
* Snapshot Isolation
    * Begin to apply locally.
    * Before commit, multicast everyone the write set.
    * If there are any conflicts, abort. Else commit. We should
      multicast to everyone, including ourselves. Then everyone gets
      this in the same order, so we know we'll all either abort or
      commit the transaction the same.
    * Note that since reads don't affect SI, we can accept read
      requests at any of the replicas without any communication with
      the other nodes.
    * If we want serializability, then we need to send the *read set*
      as well. And we would have to do that for *any* transaction, at
      any site, even if read only.
* Cluster vs WAN Replication
    * In a cluster, on a LAN, prefer eager techniques. Messaging is
      not that costly here.
    * In a WAN, you're trying to improve response time via global
      replication. Here is when you may want lazy update anywhere
      replication.
    * That can be more palatable if you can partition and put
      primaries close to writers.
    * If you can do that, lazy primary replication has staleness but
      not inconsistency.
* Degree of Replication
    * Replicating at every node in a cluster just means you aren't
      scaling writes out. That helps read throughput, but maybe you
      should limit the number of replicas.
        * But then of course now you have problems with cross
          partition transactionality...
    * Having a replica of the whole database at every global site
      imposes big communication delays for updates.
* In Practice
    * Primary copy with eager or lazy replication is common.
    * Lazy replication is common for WAN replication.
    * Few eager update anywhere protocols in practice.

**Summary**

* Eager replication from master: 1SR, but no scaling of write load.
    * Simplest choice for when read-load is high, but write-load is
      low.
* Eager replication primary copy: 1SR, need to use something like
  global 2PL/2PC to achieve 1SR.
    * Deadlock is now possible.
    * Won't scale write load if you keep a copy of all rows at every
      site.
        * Because you'll have to update all of them on every write!
    * But if you keep a fixed number of replicas (e.g., 3), then as
      you increase machines, then you can increase your write
      throughput.
* Lazy replication

Sources:

* Database Replication: A Tutorial (Kemme et al, 2010)
    * This was a great resource and much better than this joker's book.
    * https://pdfs.semanticscholar.org/c15c/1921f07cd6647d3db24babcbaff451674e16.pdf

**ROWAA: Bernstein**

* Lazy replication operates as normal in face of failures. The updates
  are propagated through the system over time, but conflicts may
  arise. But that's always the problem with lazy replication.
* Eager replication using ROWA cannot provide availability, since if
  even one site is down, we lose availability.
* We could try to just write those available sites. This is called
  ROWAA(vailable).
    * But obviously you're going to lose 1SR.
    * Suggests don't let anyone read a site that has failed and then
      recovered until it gets itself up-to-date with what happened
      elsewhere.
    * But he says we'll see that doesn't work. For one, what about
      network partition?
* Talks about immediate vs deferred writing
    * As in, with eager replication should the writes be sent
      individually as they are performed in the TX, or just as the
      very end.
    * Deferred uses fewer messages and can be done as part of the
      start of 2PC.
    * But he notes that immediate writing reduces opportunity for
      parallelization.
    * Also mentions that conflicts will arise later with deferred
      writing, after you've done more work.
    * Mentions that you can avoid this if using a primary copy; then
      write-write conflicts can be eagerly detected.
    * But I think you should still be able to have read-write
      deadlock...
    * So unless you do all writes *and reads* at the primary, you can
      have late deadlock. But then you're not distributing work across
      replicas...
* He talks a lot about ROWAA
    * *I don't understand this.*
* He also discusses quorums
    * He notes that even to read you need a quorum. You can't just
      read an available copy.
    * Because what if it's modified in another partition?
    * You may say "my read of this copy will be ordered before that
      write".
    * But what if, simultaneously, you're writing something that is
      being read (from one available copy) in the other partition!
    * So you can't get 1SR without read quorums.
    * You really need that `V_R+V_W>N`.
* Quorums will limit availability. Maybe business needs higher
  availability.
    * Maybe your system can automatically notice read-write conflicts
      from different components (he seems to assume that we outlaw
      write-write conflicts by requiring a write quorum, just not a
      read quorum).
    * He suggests then we could rollback the transaction, then redo
      it. But of course that will result in changes, which may not be
      acceptable.
    * He suggests maybe we should run validations on the data, and if
      there is any constraint violation, ask for human (or rule-based)
      intervention.
    * Such intervention could be application specific. Conflicts that
      don't result in constraint violations would be accepted.
* He mentions the typical Quorum Consensus approach:
    * Read the highest numbered version. Write a version one higher
      than the highest version you see.
    * He mentions that recovery is easy: you don't have to do
      anything.
    * A majority of the nodes will always have the most recent version
      (since this was written to *more* than a majority!). So when a
      node misses a write, it doesn't need to do anything when it
      comes back!
* But the problem with QC is that it does so much reading. Especially
  cross-site. In applications which are read heavy, this is a problem.
* Missing Writes
    * During normal operation, just you ROWA.
    * But if there are communication failures, fall back to QC.
    * If we do this, we only pay for QC when there are communication
      problems. Which is exactly when we need it, of course.
    * The idea is to fail transactions and restart in failure mode
      (using QC) if we realize the transaction is reading data that
      has been updated elsewhere, but not at the site it wants to
      read.
    * A transaction can learn about a missing write as soon as it
      tries to do ROWA and doesn't get an acknowledgement from a
      copy.
    * Now this may be fine. But imagine if `T1` isn't able to write
      `x_B`, but does write `y_A` and `y_B`. Then if `T2` reads `x_A`
      and `y_A`, we're going to have a problem.
        * If `T2` didn't read `y_A`, it could just be ordered before
          `T1`. But because it did, it needs to come *after* `T1`, and
          that `T1` made changes (to `x`) that `T2` doesn't see.
        * Therefore, `T2` needs to run in quorum mode.
    * To do this, `T1`, when it reads or writes any row, should also
      record all rows where missing writes may have occured. These are
      then propagated to anyone who subsequently touches these rows.
    * Eventually, when partitions heal, we should bring all copies of
      a record up-to-date. At that point, we should tell everyone that
      they don't need to do QC for us anymore.
    * That will involve messaging everyone; because we could fail
      again in the meantime, our request to turn off QC will have to
      be versioned.
    * I think you could also do this "lazily" by just setting a flag
      at the replicas to tell users they don't need to do QC anymore.

Sources:

* Concurrency Control and Recovery in DB Systems (Bernstein)
    * https://www.microsoft.com/en-us/research/wp-content/uploads/2016/05/ccontrol.zip
* Are Quorums an Alternative for Database Replication
    * https://pdfs.semanticscholar.org/bb2a/690fff5db6022057136442af0323215cb8a9.pdf
