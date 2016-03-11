## Primary Key Value Lookup

If there is no index on the pkey, then looking up a record by pkey is
`O(n)`, obviously.

An alternative is to use a b-tree index. Then lookup is `O(log
n)`. Another alternative is hash index. Lookup is `O(1)`. These same
time bounds apply for insertion.

## Primary Key Range Lookup

A hash index is `O(n)`, obviously. OTOH, a b-tree is `O(log n)` to get
to the start of the range, then runs linear in the number of records
to load.

## Clustered Index

A clustered index helps range queries with B-Trees. If the B-Tree has
references to a heap, then we'll need to load heap disk pages to
collect the records. That means random page access, which is slow.

By clustering, the records are right there in the table.

## Secondary Indexes and Clustering

The choice of whether to store records in a heap or clustered impacts
performance when looking up in secondary indexes. If the table is
heap allocated, the secondary index can store a direct reference to
the record's position in the db files.

If the table is clustered, records are always moving around. In that
case, a non-clustered index stores the cluster key. It must do a
lookup in the clustered index, which is `O(log n)` additional
overhead. That doesn't matter for single records because lookup in the
secondary index is already `O(log n)`.

## Range Lookup in a Second B-Tree Index

Assume we have a clustered table.

Consider range lookup. It's `O(log n)` to find the first record, then
`O(k)` to get the ids of all the records in the range.

But then you have to look these up, each of which takes `O(log n)`
time. This implies a time complexity of `O(log(n) + klog(n))`. If `k`
is too large, you'd be better off doing a full table scan. Also, you
have poor cache performance, since the items in the range don't
necessarily live near each other.

If `k` is not large, you can sort the pkeys, and iterate intelligently
through the tree, instead of starting at the root each time. It's
possible this is worth it, but there are a lot of considerations. Is
`O(k log k)` worse than `O(n)`? Will the records be dense enough
across the table that the extra work is worth avoiding having to start
at the top of the b-tree each time? In any case: one advantage of this
is that we touch pages in optimal order.

## Avoiding Clustering

Assume we a heap table. Our secondary index has direct pointer to the
records.

It's `O(1)` to look up a record, so without clustering we have `O(k)`
lookup. However, this involves a bunch of seeks. It is possible that
we might improve this by (1) sorting the memory accesses and (2)
loading in order. That leads to `O(k log k)` behavior.

It sounds like if `k` is large, you might as well do a full table
scan, because sorting would suck.

## Takeaways

A couple thoughts.

First, clustering speeds up cache-performance of one type of range
query. It slows down all other secondary index queries. That's super
extreme.

Second, b-trees are stupid unless you do range queries. Otherwise,
you'll have faster access time with a hash.

## Joining

B-Trees are competitive with hash maps for joining. With a hash map,
you iterate through the smaller hash map and lookup keys in the other
hash map. With B-Trees you can do the two-finger approach. The time
complexity is `O(min(m, n))` vs `O(m + n)`.

If one set is much smaller, you can do `O(mlog(n))` B-Tree lookup,
which may be better.

## References

* http://use-the-index-luke.com/blog/2014-01/unreasonable-defaults-primary-key-clustering-key
