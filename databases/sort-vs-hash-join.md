If a relation is not already sorted, it may be more efficient to do a
hash join because this is `O(n)`. Depends on whether the hash table
can fit in memory.

However, if the relation is already sorted, then I think a sort join
is typically faster.
