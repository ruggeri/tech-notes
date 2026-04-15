## Test-and-Set vs Test-Test-and-Set vs QueueLocks

Before doing a TAS in a spinloop, try first doing a test that it's
even the value you're interested in. Otherwise, there's no need to do
a lock of the bus to set the value. This reduces overhead.

You can also do an exponential backoff to reduce contention.

Or instead you can do a queue lock. That way you don't even need to do
pointless spinning. This is basically a wait queue.

Also showed condition variables (basically just used a `Condition`
class that must just sleep the thread until signalled), re-entrant
locks (just a lock with a count, you just check your thread ID as you
reacquire), and semaphores (again with a count, you use `Condition` to
sleep the thread).

Also had a chapter on condition variables.

## Locking for Linked Lists

p200-p213

First shows a list with a lock in the list class. You lock the entire
list when you want to add or remove from the sorted list. He calls this
"coarse grain locking."

It then shows how to do add/remove to a LL with "hand-over-hand"
locking. He calls this "fine grain locking." To insert between two
nodes, you first get a lock on the predecessor, and then on the
successor. When you have those two locks, you can insert between them.
BTW, you only need a lock on the successor if you plan to support
deletion.

To delete a node, you need to have a lock on first the predecessor and
then the node to remove. You must lock the predecessor because you plan
to update it. But you also must lock the current node, because otherwise
someone could update the node concurrently (by deletion of
`current->next`).

Showed an optimistic version that doesn't do hand-over-hand, speeding
through to the point of the add/remove (between `pred` and `curr`). Only
then does he lock `pred` then `curr`. Because `pred` could have been
removed concurrently, or an item placed between, he rescans from `head`,
looking for `pred`. If it is never found, it must be deleted, and the
insert is retried. If `pred->next` has changed, we should also retry
(anyway, that's the simplest logic)e.

One bother with the optimistic version: I feel like you should keep a
bit to mark a node deleted. Then you will avoid the need to rescan
unless `pred` really _was_ deleted. In fact, I think they _do_ do this,
and call the class `LazyList`.

## Hash Maps

Talks about coarse and fine-grained locks on buckets. Talks about
split-ordered list.

In Cuckoo hashing, we place an item at its position. If someone is
there, we move it forward to its next position. We continue until we
find a free cell.

Here's a key. We don't move an item forward in a cycle of
locations. Instead, we move it from one possible location to a 2nd
possible location. Next time we try to move it, we'll move it back to
the first location.

When we detect a loop, it means it's time to resize. This guarantees
`O(1)` WC lookup, at some additional cost of jumping around when
inserting (still expected `O(1)`). Prolly has mediocre cache
performance.

It looks they bring up Cuckoo just to show an example where we need to
coordinate a long sequence of swaps. It isn't amenable to a lock-free
version. Bah.

No discussion of a lock-free, open addressed version of a hash map.

## Skip List

See my notes in `esoteric-data-structures.md`.

## Priority Queue

The book contains some info on priority queues. One easy way to write
one is to use a skip list! The only trouble is removing the min. That
takes `O(1)` time to peek at. You can easily mark it as removed in
`O(1)` time. Then you waste `O(log n)` time updating the heads of the
lanes.
