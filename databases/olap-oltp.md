## On-Line Transaction Processing

OLTP is about processing a lot of short transactions, each doing
queries or updates. An example is a credit card transaction
processor. Throughput is the most important metric to use when
evaluating OLTP systems. OLTP systems typically have many simultaneous
users.

## On-Line Analytics Processing

OLAP is for answering complex queries with low latency. A good example
is a scientist who wants to create queries on the fly to test out
hypotheses. Typically the number of complex queries is not very
high.

A normal database may not be able to answer the query quickly if there
is a lot of data to work through. Therefore, we may want to
parallelize the work across many machines to get the answer faster.

OLAP systems will typically sacrifice throughput if it can process
fewer queries faster. If aggregate throughput is essential, a system
like Hadoop may be more efficient.

OLAP is sometimes synonymous with DWH, or Data WareHouse.

## SMP vs MPP

SMP means means *symmetric multiprocessing*. This means that all
processors are connected via a bus to shared memory. SMP is often
contrasted against *NUMA*, where some processors are closer to some
memory than others.

For purposes of contrast with *massively parallel processing*, SMP
means any shared-memory multiprocessor system (NUMA or otherwise). MPP
means a massively parallel system, by which they mean a shared-nothing
system where machines communicate over a network. MPP systems can be
easily extended (by hooking more machines up to the network), while
SMP machines are limited by bus banwidth and contention.

## SMP/MPP and OLAP/OLTP

SMP appears to just mean a normal machine with multiple processors or
cores. Typical database solutions are SMP. They can utilize the
additional cores, but can't scale forever.

Most MPP systems are for data warehouses or OLAP. They are either
meant for high throughput or fast execution of complex queries.

Examples of MPP OLAP systems are Greenplum and
Teradata. Column-orriented DBs are popular here: Redshift and HP
Vertica (Stonebraker's commercialization of C-Store). MonetDB is
another OLAP column store.

Examples of MPP OLTP are VoltDB and MemSQL. These are in-memory
databases. Clustrix is an example of an MPP database for OLTP. CitusDB
claims to be good at both OLAP and OLTP.

## Why So Few MPP OLTP Systems?

* Sounds like mixed workloads are hard.
* TODO: WHY? Maybe Ozsu will explain!

## Parallel Vs Distributed

* Parallel databases are typically within a datacenter.
    * Goal is higher performance.
    * Relies on fast interconnect.
        * InfiniBand is much faster than disk.
    * Joins involve all machines, and in particularly approximately
      the same amount of I/O.
    * But it is done in parallel, so greater throughput and faster
      response time is possible.
    * Costs of communication are typically not relevant.
    * Can also achieve better redundancy by duplicating data across
      machines.
* Parallel DB software appears hard to write. Many solutions are
  closed-source and expensive.
* Distributed DB is for multi-datacenter systems.
    * More reliable.
    * Possible faster access to locally stored data.
* Distributed DB Implementation Thoughts
    * 2PC for locking invites availability problems.
    * Writing: can try to write to all copies, using 2PC. But then
      failure of one site reduces availability.
    * Quorum: write to a >n/2 nodes. But then a read ought to read at
      least n/2 nodes to make sure it is seeing all writes!

## Sources

Excellent Clustrix paper:

http://www.clustrix.com/resources/white-papers/why-traditional-sql-databases-fail-to-scale-writes-reads-effectively/
http://www.clustrix.com/resources/white-papers/how-clustrixdb-rdbms-scales-writes-and-reads/
http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.40.6918&rep=rep1&type=pdf
* Distributed and Parallel Database Systems, Oszu
