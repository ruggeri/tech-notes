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
* They also talk about what to do when you recover.
    * Mostly, we just pick up where we left off.
