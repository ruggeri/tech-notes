Looks like it's a form of lock-free programming.

Mutex typically has a reader or writer block any other reader or
writer. A reader-writer lock has readers block writers, and writers
block both readers AND writers.

Here, only writers will take locks; writers don't block readers. The
writer can make a change, doing an atomic operation to finish the
change.

The trouble is if the writer needs to deallocate the original. In a
list, this happens when we want to do a remove, or a modification (to
do a modification, we need to create a new version of that node, then
atomically modify the prev).

The trick here is all other threads using the structure use
`rcu_read_lock()` and `rcu_read_unlock()`. The contract is that if you
are reading the shared structure, you have to read lock it
first. References must not persist after the unlock.

The writer will wait until everyone has unlocked, at which point it
knows no one has a reference to the structure to be deallocated.

It has a simple means to do this; it waits until everyone has unlocked
the structure at a time after the modification. It can do this by
looking at shared state timestamp (one per thread).

Basically, it sounds like RCU is about making lock-free algos possible
without a GC.

Like any lock-free algorithm, the reader must be careful that the
shared structure can change from underneath them.

This is particularly useful when there are many more readers than
writers.

## Relativistic Hash Table

An extensible, sort-of-lock-free algorithm (only single-threaded for
writers).

So you use chaining, and buckets live in an array. To shrink, you make
a new array of half the size, and point each bucket to the head of
half the buckets. You take the last of the chain, and point it at the
head of the bucket it should merge with.

(For this to work, you need to always have a power of two buckets, so
that buckets are merged on resize, or split. They are never
redistributed to many other lists.)

Now, when shrinking, for a moment you'll have combined buckets. That's
okay, because if you're looking for a key, you don't care if you
iterate through 2x the size of the bucket.

Let's consider growing. You allocate a new array twice as big. Point
each bucket at the first item in the old bucket that belongs here now.

Now you can start iterating through, splicing out unneeded nodes in
each bucket.

They may have a few tricks to allow simultaneous
insertion/modification while we are resizing. But I'm not super
interested...
