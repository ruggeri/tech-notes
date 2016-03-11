## Test-and-Set vs Test-Test-and-Set vs QueueLocks

Before doing a TAS in a spinloop, try first doing a test that it's
even the value you're interested in. Otherwise, there's no need to do
a lock of the bus to set the value. This reduces overhead.

You can also do an exponentional backoff to reduce contention.

Or instead you can do a queue lock. That way you don't even need to do
pointless spinning. This is basically a wait queue.

Also showed condition variables (basically just used a `Condition`
class that must just sleep the thread until signalled), re-entrant
locks (just a lock with a count, you just check your thread ID as you
reacquire), and semaphores (again with a count, you use `Condition` to
sleep the thread).

Also had a chapter on codition variables.

## Locking and LL

You can do add/remove from a LL with "hand-over-hand" locking. Showed
an optimistic version that doesn't do hand-over-hand, speeding through
to the point of the add/remove, then locking these, then speeding
through, seeing if it missed any removals. If not, then you can insert
here.

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
coordinate a long sequence of swaps. It isn't ammenable to a lock-free
version. Bah.

No discussion of a lock-free, open addressed version of a HM.

## Skip List

It's a sorted structure. When you insert, you randomly choose
height. You're in level `i` with probability `p**i`. This works out to
allow expected logarithmic lookup.

This works in the face of deletes, which are themselves random.

Inserts/removals don't require rebalancing, or any global
restructuring, so this competes in parallel applications with BST.

It seems easy to, when inserting, to slowly add the link at each
level in a lock free way. Indeed, it is.

## Priority Queue

The book contains some info on priority queues. One easy way to write
one is to use a skip list! The only trouble is removing the min. That
takes `O(1)` time to peek at. You can easily mark it as removed in
`O(1)` time. Then you waste `O(log n)` time updating the heads of the
lanes.
