Most of this stuff seems to also be covered by Use the Index, Luke!

## Single Pass Algorithms

* Filter or projection have obvious one pass algorith, read in and do
  the filter or projection of each row, and output this.
    * Unit memory, linear in relation size.
* Duplicate elimination
    * Read things in one at a time, store in hash table or BST.
    * Hold entire relation in memory, linear.
* Grouping
    * MIN, MAX, COUNT, SUM, and AVG all have obvious one-pass
      solutions, that take unit memory per group.
    * Hold entire relation in memory, linear.
* Union and intersection can be done in the obvious way: read in an
  entire relation, build a hash table or BST, and then read the other,
  filtering as appropriate.
    * Uses as much as memory as smaller relation, linear IO.
* Likewise, join can be done this way.
* One approach to handling a join where neither relation can fit in
  memory is to do a nested-loop.
    * Notice that a simple loop is not the smartest way to do this.
    * Instead, read a block of `R` and a block of `S`, do the join
      here, with all the `r` in `R` and `s` in `S`.
    * Then read the next block of `S` and repeat.
    * This way you aren't needlessly thrashing the cache.
    * Constant memory (more helps), quadratic time (but inversely
      proportional to the anount of memory).

## Two-Pass Sorting Algorithms

* For larger relations, we cannot hold everything in memory.
* Basically, sort each block in memory, write each sorted block to
  disk. Then iterate through this, using the front of each block.
    * This is external sorting, but you don't necessarily have to do
      the merge, if you just want to iterate in sorted order.
* Duplicate elimination can now be done in constant memory.
    * Takes `O(n log n)` time to sort, of course, followed by `O(n)`
      extra work.
* Likewise grouping and aggregation.
* Doing a union or a set difference is also simple, by zippering with
  two fingers.
* And a join can be likewise done.
    * They recommend a trick. When doing the join, we would might
      naively do the merge of external merge sort. We'd write back the
      merged data. Then we'd iterate through each totally sorted
      relation, using only use a single buffer from `R` and `S` to
      join (containing the "next" stuff).
    * Instead, sort each block of `R` and `S`. Instead of merging,
      take the min of any block of `R`, and compare it to the min of
      any block of `S`.
    * This avoids unnecessary IO, by using the memory more
      intelligently.

## Two-Pass Hashing Algorithms

* To duplicate eliminate, proceed one by one, hashing everyone to a
  block. That's linear time. Then do the linear time algorithm to
  eliminate duplicates in this block.
    * This works for aggregations, too.
    * Linear time, works in any amount of space you have.
* Same idea works for union, intersection, and difference.
* And again for a join.
* These seem to make a better use of memory:
    * Once you have the data sorted, then everything is easy. You can
      just do things with `O(1)` memory and linear time.
    * But if you hash, we will require more memory (since blocks are
      unsorted), but it still is linear time.
    * So why waste time sorting if we have the memory?

## Index Based Algorithms

* First, they distinguish between clustering and non-clustering
  indexes. A clustered table admits a single clustering index.
* A selection based on an attribute in an clustering index just needs
  to search the index, then we load the appropriate block.
    * This also works for ranges if the index is a B-tree.
    * Selections made up of chains of equality or range conditions can
      benefit.
* Doing an aggregation can benefit from a clustering index.
    * With a non-clustering index, you'd have to do a lot of random
      IO. You're probably better off using a hash algorithm.
* When joining `R` and `S`, and an index exists on `S`, we can load a
  block of `R`, look for those tuples in `S` that join, and then load
  those.
    * Regardless whether the index is clustered, we're going to have
      to do a lot of random lookups. That means a bunch of
      IO.
    * Prolly better to do a hash join.
    * OTOH, this can make sense if `S` is huge relative to `R`.
    * This is better than sort or hash joins, which will require use
      to look at all of `S`.
* If an index exists on both relations, so much the better.
    * Maybe both relations are quite large, requiring a sort or hash
      join.
    * Instead, we can do this with the indexes. Now we know which
      records to join.
    * We'd typically have to do random IO to actually pull in the
      joined records and produce the joined row, though.
    * So this would only really help if the number of joined records
      is small.
    * It might sort of be possible to try to reduce the randomness of
      the IO, but rows really can be joined quite randomly.
* Things are slightly better if we have two sorted (e.g., B-Tree)
  indexes.
    * Then we are in a good position to do a sort join, but no
      actually sorting needs to happen.
    * We can just do things two-finger style. This is linear time and
      requires only constant memory.
    * This is sometimes called a zig-zag join.
* If the indexes are in fact clustered, then no random IO is needed.

I've probably missed some tricks here, but everything they covered I
already really had experienced via Use the Index, Luke.
