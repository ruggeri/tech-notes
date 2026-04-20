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

They will now implement an "open-addressed" hash set. This won't have
chaining. They will use a two-table cuckoo-hashing approach (see
`hash-map-tricks.md`), and make it concurrent. But they won't make it
lock free; they will use locks.

First, they won't really make this open-addressed. They are going to
make buckets at each index, which they call "probe set." The idea will
be that probe set size will be limited to `PROBE_SIZE` elements
temporarily, but on quiescence will be no more than `THRESHOLD`
elements. Possibly `THRESHOLD` could be as small as one?

I believe the need for `PROBE_SIZE > THRESHOLD` is because two
concurrent insertions might both try to put an item in a slot. If a long
chain of cuckoo relocation work has been done, we don't want to
immediately reverse course.

When `THRESHOLD` is exceeded, you will `get` first item in the probe
set. You will then lock both tables for this item (first table0, then
table1; must lock in order to avoid deadlock). Then, double check the
item was removed from probe set by a concurrent thread; keep retrying.
If not, you can move it! Then, release locks. If you moved somewhere
with more than `THRESHOLD` items, keep moving things forward.

Last, we will trigger a resize when moving an item from the probe set
would violate the `PROBE_SIZE` of where we would move it to. This is
going to be a stop-the-world resize, claiming all locks in the table.
They show striped and "refinable" versions.

I believe I have captured the main ideas, but I didn't _super_ closely
interrogate what they were doing. Maybe I was unfairly biased against
the probe set idea. You can probably do this all in contiguous memory,
since probe sets have fixed maximum size, and you can CAS a length
variable. Their indirected `ArrayList` probe set does make their
presented version _closed_ addressed, but they probably only did that
for simplification. If you inline the probe sets, it is fair to call
this open addressed.

This code isn't lock free. It seems impossible to remove the locks,
because when moving an item from one location to another, there must not
be a moment where it can be "missed" by a concurrent reader. If you need
lock-free, then the split ordered list is better than this.

Also, you eventually need to reallocate the tables and rehash
everything. This will stop the world, and I don't see any obvious
workaround. ChatGPT suggests something about "epochs".

I will study open-addressed concurrent hash maps more in my
`concurrent-hash-map.md` notes, which have a goal of presenting a
lock-free, open-addressed, resizable hash map.

## Note On Multi-Update "Transactions"

Just because you allow single concurrent updates to a map doesn't mean
you can extend the idea to support _multiple update_ transactions
easily. To do so would require modification to/support from the map
class itself.

For a map with locking: if you just held the locks from an earlier
operation and keep them until the end of the transaction, you could
easily incur deadlock with other concurrent operations. If you had a
lock-free approach, there would typically be no way to avoid leaking of
state mid-transaction.

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
