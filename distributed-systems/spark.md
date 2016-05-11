## Introduction

Broadly similar to Hadoop in many ways. Files are distributed; jobs
run, they send tasks to executors on worker nodes.

Sounds like a major distinction is that Spark tries to keep files in
memory, so that iterative computation is faster.

They give you a shell; that's kinda nice. You manipulate RDDs
(*reliable distributed datsets*; which is just a distributed file that
might be in memory) with operations like `filter`, `map`, `count` et
cetera. The functions you pass into these get shipped to the
executors, I guess.

Color me moderately interested in how a function object is serialized;
especially if it captures variables. It sounds like this is possible
if the closed-over variables are themselves Serializable, which
exactly makes sense.

An example in Java is like this:

```
// sc is the SparkContext object.
JavaRDD<String> lines = sc.textFile("myFileName");
JavaRDD<String> words = input.flatMap(s -> Arrays,.asList(s.split(" ")));
JavaPairRDD<String, Integer> wordCounts = words.mapToPair(w -> (w, 1)).reduceByKey((c1, c2) -> c1 + c2);
wordCounts.saveAsTextFile("outputFileName");
```

I'm writing my own fake version of Java here. If the reduce operation
assumes associativity, then a combiner can presumably be applied at
the map site. Then a shuffle is presumably inferred.

The use of lambdas here, and the implicit topology of jobs, is a major
convenience over traditional Hadoop, where a job class needs to be
written no matter how trivial the operation.

Transformations are executed lazily, so that we can combine operations
that can all be done at one site with one pass of
data. *Transformations* lazily produce new RDDs, *actions* require
materialization, and actually cause work to be done.

By default, we don't persist intermediate results, but you can ask
Spark to do this by calling `#persist()`. This helps if you will reuse
that computation. You can persist to memory or disk.

It is typical to persist a dataset that you are working with
interactively, so that it stays in memory for the duration of the
session. Presumably it is mapped out of memory when the session is
done.

## Common Operations

Transformations:

* Element-wise:
    * filter, map, flatMap
    * Sample
    * These are map-only.
* Set operations:
    * union
    * distinct
    * intersection
    * subtract
    * Distinct/intersection/subtract require sorts.
    * cartesian product
        * Involves sort, obviously produces many more records, so be
          careful!

Actions:

* Reduce
    * There's also fold, which is more general.
    * Can also use aggregate, where you (1) tell how to add a record
      to an accumulator, and (2) tell how to add two accumulators.
* Collect
    * Brings down the entire dataset to the machine.
* take and takeSample (pulls a limited sample down to the machine)
* count and countByValue (as a convenience; note this is an action,
  not a transformation)
* saveAsTextFile, saveAsSequenceFile

As mentioned, we can use persist if we don't want something to be
recalculated. This is basically caching a result. There are a number
of modes:

* MEMORY_ONLY
* MEMORY_ONLY_SER (serializes to try to save space, presumably through
  compression; costs more CPU).
* MEMORY_AND_DISK (spills to disk as needed)
* MEMORY_AND_DISK_SER
* DISK_ONLY

Spark apparently will evict cached data, so while using too much cache
won't cause a program to fail, it will however degrade performance.

## Key/Value Pairs

You can partition by key using `mapToPair` or `groupBy`; presumably
`mapToPair` is a convenience.

**Map-Only Tasks**

* `reduceByKey`, `foldByKey`, and `groupByKey` (just builds an array of values)
    * Both `reduceByKey` and `foldByKey` wil do combining for
      you. That seems not totally logical for fold, but fine.
    * If you need more control, use `combineByKey`, with which these
      others are implemented.
* `mapValues`, `flatMapValues`

I assume these are map-only tasks, since the data is already
partitioned appropriately. A big benefit: Spark will remember that how
your data is partitioned, unlike, for instance, `map`, which will
forget that. This makes sense because `map` could theoretically
transform or even just drop the key.

So use these so that Spark can exploit this.

**Joins**

* `join`, `rightOuterJoin`, `leftOuterJoin`.
* `cogroup` (gives you `(key, [leftVals], [rightVals])`).

If one of the datasets is already partitioned appropriately, then data
for that set can stay where it is, and we do a shuffle on just the
unpartitioned dataset. Then we do a merge join.

If both sets are partitioned, *and* they use the same number of
partitions, then all the data is where it needs to be, and you should
just be able to do a merge join right here.

Okay, that's almost true. Of course, your data may be placed on
different nodes of the cluster, even if you use the same number of
partitions. But that Spark will try to colocate data produced from
upstream jobs if it sees that is useful downstream. And surely there
must be a feature like `seed` that allows you to force it to colocate
data.

If it's colocated, it does exactly what I say; it avoids any network
transfer. This is very important for *iterative* algorithms; they give
PageRank as an example.

```
links: source => [destinations]
scores: source => score

join links and scores (local join)
map to (destination, score / len(destinations)) (local)
reduce to (destination, sum score /len(destinations)) (combine + shuffle)

Now we're ready to do the next iteration. So one shuffle is needed per
iteration, which I wouldn't see how to avoid anyway.
```

Of course, merge joins do not require mapping the entire dataset into
memory.

**Repartitioning**

You can set the level of partitioning with most of these operations;
Spark will try to infer a reasonable default if you don't. You can
always `#repartition`, but that is of course expensive. I think if you
want to reduce partitions, you can use `#coalesce`, which presumably
minimizes data movement, maybe.

**Actions**

* `countByKey`
* `collectAsMap` (pulls down as a HashMap)
* `lookup(key)` (queries the values for a key).

## Loading/Saving Data

Common formats are:

* Text
* CSV
* JSON
    * You have to parse yourself.
    * Can directly hydrate a classes' properties from JSON.
    * Obviously no compression, other weaknesses of JSON.
* SequenceFiles
    * Primitive types plus arrays and maps.
    * Still bullshit.
* ObjectFiles
    * Uses Java's serialization.
    * Serialization can be slow.
    * Java serialization/deserialization can be a pain with that
      serialization field thing.
* Can use compression of course.

Can use local filesystem, HDFS, or even S3. S3 can be fast if you're
running in EC2.

You can use a database as a source; you specify a query, and it will
create a RDD out of the results. Ideally you'll find a way so that
multiple Spark nodes can simultaneously extract different ranges of
data, so that you parallelize your extraction from the DB.

Likewise you can pull data out of Cassandra and HBase. You can also
pull data out from ElasticSearch; not sure what that would mean,
actually.

## Advanced Spark Programming

**Accumulator**

Accumulators are event counters. Just like at QC, the updates are
batched and sent in a different thread.

They mention that Spark restarts tasks, and even launches speculative
workers if it notices someone is taking a long time. You may even have
to rebuild results if they are cached in memory, but get evicted.

This can possibly cause problems with accumulators, since they'll get
incremented again. To avoid this, it's only safe to increment
accumulators in an action like forEach, which is guaranteed to be run
only once.

Actually, it seems like in a forEach I might process the same row
twice if a task fails. But this is an example of a side-effect which
presumably can be buffered and sent at the end of the task.

I don't know, you do want the accumulator inside a transformation
prolly. They say this inaccuracy prolly doesn't matter since you're
using accumulators for debugging really.

You can even make custom accumulators like `max`. Preferably the
accumulator action is commutative, associative, and ideally
idempotent (like max).

**Broadcast Variables**

This is the way you send out a bunch of parameters to every worker. It
won't be serialized in your mapper/reducer code that's sent. It's
distsributed more efficiently. Also, it won't be serialized in each
function that references it; it will be sent only once.

Obviously changes to this data structure are local only. It's not a
means by which workers can communicate with each other.
