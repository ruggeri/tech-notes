# Test-and-Set vs Test-Test-and-Set vs QueueLocks

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

**TODO**: I wish I had some better notes from this section?

# Locking for Linked Lists

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

# Queues and Stacks

p223-p241 and p245-p255

They have a few chapters on queues and stacks. Maybe I didn't take notes
because I cover this in my `lock-free-algos.md` document?

# Hash Maps: With Locks

p299-p309

They implement a version with a coarse, whole-map lock. You take a lock
on the entire hash set in order to add or query it.

They show a finer-grained version where they _stripe_ the locks. Each
lock only manages a fraction of the table. However, resize still
requires taking _all_ the locks: it is a stop-the-world operation.

They next show a version where the number of locks are allowed to grow
on resize. I believe the idea is that, as the map grows, it may have
more-and-more users, which means you want finer-and-finer lock
granularity to avoid excessive blocking. That might make sense if users
are machines on a network, since for local use you will be eventually
limited by CPU parallelization.

For performance reasons, yhey want to avoid taking locks on the buckets
array, so they will use lock-free CAS operations. Basically, someone is
going to do a CAS to claim the job of resizing the hash map. When they
do, they are going to stop everyone accessing it. They will wait for
everyone using locks currently to finish. Then they will resize the
buckets, rehash everything, and allocate new locks. This is still
stop-the-world, but the introduction of lock-free use/resize of the
buckets array is for performance.

Users of the hash map will be careful to check if another thread has
claimed resizing of the lock array. They'll then claim the bucket lock
they need. They then check two things: (1) that the locks array has not
been concurrently reallocated (else we've locked access to a bucket that
is no longer in use), and (2) that a resize operation is not underway.

I believe the second CAS operation in `acquire` is necessary to create a
"happens-before" relationship so that checking that the locks array did
not get reassigned will actually see concurrent modification.

It feels like we might be able to eliminate the check for (2) if we set
the resized locks array at the beginning of the resize (immediately
after using CAS to claim the resize). Maybe that really would work, but
we'd need another CAS on the atomic reference to make sure other threads
_see_ that we set the locks array. That's slower. And ultimately, it's
probably cleaner to handle the two cases of (1) a resize completed or
(2) a resize is in progress, rather than "smush" both those situations
into one variable.

`acquire` checks whether _it_ is the thread that has claimed the flag.
This is possibly unnecessary, but allows re-entrancy from code that has
claimed the flag, and is probably just safer.

Clearly, even this simple code is still fairly subtle!

# Lock Free Hash Sets

## Split Ordered List

p309-p316

They present a style of "closed hashing"/chaining concurrent hash map.

Basic idea is that you will have a linked list of nodes sorted by hash
key. There are two kinds of nodes: value nodes and "sentinel nodes"
which mark the start of a bucket.

Lock-free insert into a linked list is simple, and already covered.
Misordering cannot be introduced unless there is an attempt to
concurrently update a node, in which case you should retry.

You never re-order items in the list.

We are going to maintain pointers into the linked list for fast access.
They will mimic an idea from _extensible_ hashing. Basically, when the
list grows too big, you will double the buckets array size. But you will
_not_ initialize all the buckets. You will copy over the old buckets
array, but set all subsequent else to `NULL`.

To lookup a position in the values list, you first _reverse_ your key.
Then you mask the bottom `k` bits. That corresponds to a bucket array of
size `2**k`. That's sort of like looking at the top `k` bits of the hash
key. But we'll proceed in a specific order:

- Position zero: bucket for everything starting `0...`.
- Position one: bucket for everything starting `1...`.
- Position two: bucket for everything starting `01...`.
- Postion three: bucket for everything starting `11...`.

So when you double the buckets array, the old buckets positions do not
need to be updated. The new buckets are formed from "splitting" the old
buckets. This is exactly the extensible hashing idea (see
`hash-map-tricks.md`).

But you don't do the splitting right away. That would involve scanning
the entire list. Instead, just do it _lazily_; when you encounter a NULL
value in the buckets array, just walk backward to a prior bucket, and
then scan forward to find where to place the bucket sentinel. Splice the
sentinel in and update the bucket array.

You do have to track the total count, to know when to resize. They
mention in their paper that they have threads locally track the number
of inserts, and only rarely update the count. That avoids contention.

They suggest that, instead of a single dynamic array holding the bucket
starts, you might use a multi-level tree. This adds a logarithmic factor
in lookup, but if the branch/node size is quite high, you need very
little depth to map more nodes than there is memory on a 64-bit machine.
Thus, you can treat this as relatively constant.

Shalev and Shavit published it, so I think it's a unique interest of
theirs. I'm not sure if this is a widely used idea.

- https://ldhulipala.github.io/readings/split_ordered_lists.pdf
  - Split-Ordered Lists: Lock-Free Extensible Hash Tables by Shalev and
    Shavit.

## Concurrent Cuckoo Hashing

p316-p327

**TODO**: Review me.

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

# Skip List

See my notes in `esoteric-data-structures.md`.

# Priority Queue

The book contains some info on priority queues. One easy way to write
one is to use a skip list! The only trouble is removing the min. That
takes `O(1)` time to peek at. You can easily mark it as removed in
`O(1)` time. Then you waste `O(log n)` time updating the heads of the
lanes.

# Sources

- These notes are from The Art of Multiprocessor Programming by Herlihy
  and Shavit.
- I liked this book a lot. However, these notes taken at the time were
  probably too high level and not detailed enough. These notes seem to
  leave out a lot of interesting material. Possibly material was robbed
  from here and put in a variety of other concurrency related writeups,
  though.
- I left out material on stacks and queues; I believe my notes on lock
  free data structures might cover a lot of what the book covered?
- I believe my notes on mutexes cover some of what this book covers? But
  I think they cover more about how atomic operations are implemented in
  hardware? Maybe that is covered in my cache coherency notes...
