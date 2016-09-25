**MySQL Cluster**

* Sometimes called NDB, since it uses the NDB (Network Database)
  storage engine instead of InnoDB.
* It is multi-master. It shards the data across machines to distribute
  the write load.
* It autoshards the data as machines enter/exit.
* It is synchronous. Replication is synchronous. Coordination is two
  phase commit.
* Typical setup is for all data to be in memory.
    * Typical setup is to not immediately write to disk.
    * Instead, write in batches.
    * Just writes a redo log, with occasional compactions.
    * Potential for dataloss in a 2sec (default) window exists.
* Been around for a while. Everyone says it's really hard to setup,
  has lots of problems.
    * Aphyr didn't test it because he tried for a week to set it up
      and failed.
* Does do cross-shard joining, of course.
* Has ACID semantics.

**Galera**

* Galera is a library made by Codership.
    * Galera is a library that hooks in with MySQL to do sharding for
      you.
* Many variants:
    * Codership offers Galera Cluster for MySQL
    * MariaDB has an official MariaDB Galera Cluster download.
    * Percona offers Percona XtraDB Cluster
        * XtraDB is just a version of InnoDB with some patches to
          improve performance/tunability.
        * Percona offers something called Percona Server, which is
          MariaDB/MySQL plus some patches.
        * Percona also owns TokuDB, by the way.
    * All of these are *basically exactly the same*.
* Seems to be the same synchronous replication, multi-master idea.
* Both MariaDB Galera Cluster and Percona XtraDB Cluster failed
  Jepsen.
    * They failed pretty hard, actually. Did not provide consistency.
    * Percona CTO got pissy about failure.
* Not sure if this is a fundamental failing, or just a fixable bug.
* But it speaks to the quality of these products.

**Postgres Options**

* CitusDB
    * Autosharding.
    * Used to be single-master, but have just released CitusMX which
      is multi-master.
* Postgres-X2 (previously Postgres-XL)
    * Actually maybe -X2 is a fork of -XL or something?
* Greenplum
    * This was the original, I think.
    * But it was forked from an old version of Postgres and can't
      proceed.
* BDR
    * This actually just does replication between two clusters, but
      half the data is only written in one geo, and the other half in
      the other.
    * It's not a real cluster DB.

**TODO**

* I still feel unsure why we don't use shared nothing parallel
  databases.
* Why did NoSQL options win?
* But maybe that's the point: the NewSQL stuff is poised to make a
  comeback.
