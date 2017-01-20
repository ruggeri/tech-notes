SSD is still block orriented, and not nearly as fast as RAM still, so
B-Tree can still makes sense. Random reads can be fast, but not random
writes on SSD. SSD doesn't like to do random overwrites, it likes to
append. This is because SSD cannot directly overwrite; they need to
erase before writing, but erase happens in large "blocks", while write
happens in smaller "pages".

For that reason, appending is good, because you erase a block and just
keep extending.

## Copy-On-Write Trees

So a copy-on-write approach is going to rewrite a path in the
tree. What you do is just append this path into the log. You don't
need to touch the preceeding blocks and rewrite them.

This is going to require some kind of garbage collection process
eventually; you're creating a bunch of garbage by doing this. Total IO
will be greater: you have to write the whole path, instead of just one
block. It's not about modifying in place vs copying the leaf node,
since you have to write the leaf node regardless. It's about writing
the whole path.

You're also going to cause space blowup, since you're creating a bunch
of versions. That might be desirable for a versioned file system
(typical use case, historically), but could be annoying if you don't
care about versioning.

But one advantage is that CoW b-trees avoid overwriting. For SSDs,
which have trouble overwriting, this could be a win.

Source: http://www.bzero.se/ldapd/btree.html
Source: https://pdfs.semanticscholar.org/76f6/e56bbb2b860cdc75bbcb5510b1d811c9a768.pdf

## Log-Structured Merge Trees

Multiple trees. In simple case of two trees: one on disk (C1) and one
in memory (C0). Also, hot nodes of C1 can be buffered in memory.

As writes come in, you log it to an on-disk sequential log. You insert
the write into C0.

As C0 fills up, we need to migrate data to C1. This happens via a
"rolling-merge" process. Contiguous sets of keys will be written out.

They want C1 to be a B-tree, but optimized for sequential
access. Blocks are 100% full, and optimized in a way that keeps leaf
nodes in sequential order on order (for faster range queries). This is
a nice optimization: basically, your B-trees will not be fragmented.

What they do is, take a subtree of C1, and take the corresponding
subtree of C0. Merge the two in memory, and then write out to
disk. You can now drop the in memory version.

Basically, we're using the in memory B-tree to buffer and pre-sort
things. Then we write it out in time, in a way that doesn't require
random IO. **This is huge**: sequential insertion is awesome, because
all the inserts hit the same block.

Note that C0 can be organized in whatever way is easiest for the
in-memory operation. You don't need to do things B-tree style.

The bigger C0 is, the more batching is happening. But the bigger C1 is
relative to C0, the worse for batching. That's because for each
segment of C1 we bring into memory (which is fixed in size, because
memory is limited), there will be less we can merge from C0.

This motivates *multiple* levels of on-disk trees. That way you always
merge things that are roughly the same size, which is more efficient.

But that tends to slow down querying, since you have to look at each
tree. To minimize this cost, you can use bloom filters. Note: this
can't help with range queries, so that is a potential downside: it's
hard to mitigate the cost of multiple levels when dealing with
multiple levels.

BigTable has does a similar merging thing, but I think sstables just
use sorted files, without any tree structure. LevelDB is an LSM
database, and RocksDB is a fork of LevelDB optimized for SSD.

(According to Wikipedia, BigTable indeed uses LSM tree).

Note that LSM writes are basically appends, but that reads can be
slower, if only because reads are essential random. For this reason,
Cassandra suggests that you not read before write, and that you should
use UPSERT behavior instead.

## Sources

* http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.44.2782&rep=rep1&type=pdf
* https://www.percona.com/blog/2011/10/04/write-optimization-myths-comparison-clarifications-part-2/
* http://www.benstopford.com/2015/02/14/log-structured-merge-trees/
* http://smalldatum.blogspot.com/2016/01/summary-of-advantages-of-lsm-vs-b-tree.html
* https://en.wikipedia.org/wiki/Log-structured_merge-tree
