This is a summary of [Google BigTable][big-table-pdf].

[big-table-pdf]: http://static.googleusercontent.com/media/research.google.com/en/us/archive/bigtable-osdi06.pdf

## Goals

BigTable is a distributed key-value store. It splits a table up
amongst many **tablet servers**, each of which are responsible for
replying to queries and updating/inserting records.

There are no transactions, except on a single row.

## Row Structure

Each row has a:

* Row key
* Column key
* Timestamp
* Value

The keys and value are strings. Typically the row key and value are
serialized objects.

Data is kept in lexicographic order by row key so quickly find a
record, and also to support good performance when accessing nearby
keys.

Column keys are grouped into **families**, which is the unit of access
control. Column keys are of the format `family:qualifier`; the
families are pre-defined, but the qualifiers are arbitrary.

In addition to access control, at the column family level we can
specify how many previous timestamped values are allowed. We can keep
either the last `N` or those with timestamps more recent than `X`
seconds.

## Architecture

BigTable uses Chubby for coordination. When a BigTable cluster starts
up, one worker is selected as master by obtaining a Chubby lock.

The other **tablet servers** will acquire a lock in a specified
directory to inform the master of their existence. The tablet servers
will attempt to maintain this lock throughout their life. If possible,
the server will attempt to explicitly release the lock when it decides
to leave the cluster.

## Serving Tablets

The master assigns **tablets** (a range of keys) to the tablet
servers. Tablets are stored in files on GFS, which takes care of
persistence. The files are **SSTables** (sorted string tables) of
updates.

Each SSTable is organized into blocks. At the end of the SSTable file
is an index, with the starting key of each block. Looking up updates
to a row involves reading the index, finding the appropriate block,
and scanning for the appropriate record.

As the tablet server receives updates, they are written into a log,
and also inserted into a **memtable**, which is an SSTable in RAM.

Reads are performed on a merged view of the SSTables backing the
tablet, plus the memtable. Since the SSTables and memtable are sorted,
this can be done efficiently. As a refinement, Bloom filters can be
used to avoid looking at an SSTable unnecessarily.

## Compaction

Eventually the memtable grows to a critical size. It is then written
out as the next SSTable.

This grows the number of SSTables, but these too are compacted over
time. Every once in a while multiple SSTables (plus the current
memtable) are merged and saved.

All of this can happen in the background.

## Splitting Tablets

The tablet server can split a tablet if it gets too large.

Splitting tablets is easy; to split a tablet, continue to use the same
existing SSTables for both splits. Over time, they can rewrite the
other tablets' data out through compaction.

To split a tablet, updates are written by the tablet server in the
**metadata tablet** which stores the locations of all the tablets. The
tablet server stops serving this tablet and the master reassigns it.

**I'm guessing here. The paper is unclear on this point.**

## Consistency and Fail-Over

A tablet must have a single tablet server managing it. This is ensured
because the tablet server maintains a Chubby lock. If the lock is
lost, the tablet server shuts down.

The master can identify the lost lock and assign those tablets to some
other server.

## Upshot

Scales read/write load across thousands of machines. Basically a big
key-value store. Row atomicity, but no cross-row transactions.

Presumably BigTable suffers from submarine writes just like GFS. I
guess anything would: you can never be certain that an ack from a
server is received.
