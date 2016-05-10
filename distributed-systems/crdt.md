# Conflict-Free Replicated Data Type

Basic idea is that the same set of mutations, applied in any order,
will result in the same value.

I believe the idea is this: during a partition, keep writing. On heal,
merge the updates. The result is consistent with any ordering of those
updates.

In a sense, you haven't rolled back anything. There is no conflict.

On each side of the partition, you see a different order in which the
updates were applied; side A sees all the A operations, then the B
operations, while side B sees all the B operations, then the A
operations. So there isn't necessarily a globally consistent view of
the order of the transactions.

Note that CRDT never requires human intervention to resolve conflicts,
since there is no conflict on heal.

Some examples:

**Counter**

For `n`-processes to share a counter, store `(c_1, ...,
c_n)`. Increment only your own. On merge, take max of all values.

**Increment/Decrement**

Keep two vectors: `(c_1, ..., c_n)` and `(d_1, ..., d_n)`; the first
records increments, the second decrements.

**Insertion Set**

Trivial; just keep adding elements to the set, and when merging take
the union.

**Insertion/Removal Set**

Keep two sets, one for inserts, one for removes. Obviously you can
never (successfully) add after remove. If a remove happens
concurrently with an add, the remove comes last and wins.

There's a version called LWW-Element-Set, where we keep a list of
`(element, add time, delete time)`. We just choose the later of add
time and delete time. This would be exactly a set if there were a
global clock, otherwise it is a CRDT and pretty okay.

**OR-Set**

Keeps a list of `(element, [add times], [remove times])`. Element is
present if any add time is `[add times] - [remove times]` is
non-empty. To remove, you add set `remove times := remove times union
add times`.

## Sources

https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type
https://github.com/aphyr/meangirls
