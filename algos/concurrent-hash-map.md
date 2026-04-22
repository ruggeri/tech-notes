# Goals, Basic Design

These notes are about Cliff Click's `NonBlockingHashMap`, released in
2007.. He gave a lecture at Google around that time. Later, his code was
improved maybe for the H2O package.

Goal is to build a lock-free hash table, but also practical. He cares
about cache friendliness and performance, and also ability to give great
performance with many CPUs.

Basic idea is to use open-addressing. He'll use a reprobe of 1 for best
cache friendliness. The number of buckets will be a power of two for
fast MOD (since it's just an AND).

He puts the `(key, hash, value)` all in the same cache line (the hash
value allows faster equality check). There are no allocations in a `put`
operation.

When a key is set at a location, it never is deleted or changed. That
will be essential for correctness. When you `get` and find the `key`,
you can return the `val` and know that the corresponding `key` will not
change underneath you. Truly emptying a cell would also mean that later
cells might have to be moved backward if they previously couldn't be
stored at their intended cell because of prior collision.

Values are either `NULL` (not set yet), `set`, or `TOMBSTONE`.

# Operations

**Get**: lookup, check the `key`. If it's not there, return `NULL`, if
it is, return the `val` (if any is set yet). If the _wrong_ key is
there, start reprobing/walking and repeat until you find your `key` or
encounter an empty cell. NB: if memory order is ever weird and you see a
`value` but `key` is still null, that's fine, treat it as a miss. When
you find the `key`, return the `value` (unless it is PRIMEd, which tells
you to look at a new table; we'll cover that later).

**Set**: probe to the first cell that matches your `key`, or has `key ==
null`. Do a CAS on the key to claim the cell. If it fails, retry.

Once you CAS the key, you can CAS the hash (if not already set). Last,
keep CASing the value until you succeed. There is a subtlety we'll cover
later: if the value is PRIMEd, then it means this value has been copied
over and we should go to the new table.

If anyone sees an in between state, no problem. If there's an old value
there, you'll just change that. **Delete** works just like a set, but
you set the value to `TOMBSTONE`.Optimization: if your CAS of the value
fails with a new value, then just stop, because you can treat this as
someone overwrote your value! Be careful: if it is a PRIMEd value, maybe
key has just moved (not been written with new value), so should update
new table in that case...

There is no fencing. Processors may see updates out-of-order: one
processor may `set(key1, val1); set(key2, val2)`, but another unit sees
`(key2, val2)` but not `(key1, val1)`. That's allowed, and is necessary
for good performance. It's on the map user to bring more ordering to map
updates if they need them.

Possibly you wouldn't even need CAS for writes to `value` (concurrent
writes just overwrite), except that we'll need to ensure we don't
overwrite a PRIMEd values eventually.

# Volatile Values

This is just a basic reminder of what `volatile`. It tells Java that a
variable may be modified by other threads, and so must be backed by
memory and not kept only in a register. The CPU can still try to cache
the memory, so a read of the variable may not hit actually physical RAM.
But the CPU's cache coherency implementation will insure that eventually
the cache line will get invalidated so that a thread will eventually see
another thread's write to the variable.

# Resizing

Resizing without stopping updates introduces the trickiness. If you are
allowed to stop the world during resize, then everything is easy.

You have to resize when you have to reprobe too many times. It's good to
keep track of reprobes because (1) long chains of reprobes can occur
when using power-of-two (rather than prime) open-addressing, (2) hash
function may not be very good, (3) we're not freeing cells on delete. I
believe they don't check on the size of the map, because this is slow to
compute, but instead just resize as soon as reprobes becomes too long.

Detail: map size. every insert/delete needs to increment a count. You
CAS a count variable, but it will be hot. Click creates several
counters, to distribute writes to several cache lines. To get the actual
count, you do a read across variables, which may entail many cache
misses. So he also gives a fast `estimate_sum` which basically caches an
old count.

So, on too many reprobes, you will allocate a new table, and CAS a
variable for a nested KV store. In fact, multiple resizes might be
simultaneously occurring, but we'll ignore that. To avoid every thread
allocating a new large backing array simultaneously, threads will pop a
counter of resizing threads, so that after two have popped this, other
threads will wait briefly and check if a new table got published. But
they won't wait indefinitely (block); if they don't see the new table,
they will allocate and try to CAS the nested table field.

Once a thread sees the new table, it will stop writing into the old
table, and instead try to migrate old values to the new table.

**Get**: Once you have seen a new table is published, a get will first
check the old table. If it sees a PRIMEd value, it knows that the value
for the key, if any, is supposed to be moved to the new table.

If the value is not yet PRIMEd, the first thing to do is to CAS it
PRIME. This immediately stops all updates for this key in the old table.

If the old table value was TOMBSTONE, we don't copy it over to the new
table. Else, the reader attempts to write the old table value into the
new table.

If it sees a value in the new table, it should not overwrite this
value, because a concurrent write may have occurred. If it doesn't find
the old value, it means we really are the first to copy this.

If the value was already PRIMEd, then we expect to find the key in the
new table. But it is possible that this copy-over work was never
completed (thread got slept or maybe even crashed). If so, help copy
over the old value. This is similar to the idea of advancing a lagging
tail value in a lock free queue. This raises the possibility of finding
the old value already copied over. That is no problem: that means
someone else concurrently helped us finish the copy over.

After copying over the value, go ahead and write a PRIMEd TOMBSTONE to
the old table. This shows that the old value has already been copied
over. You don't have to CAS this one, since no one else could have
changed the old PRIMEd value to anything other than PRIMEd TOMBSTONE.

Last, you can finally get the value in the new table.

**Set**: It mostly works the same as `get`. If you know a new table has
been published, copy over the key/value with the same logic as before.
Only, instead of copying over the value to the new table, you can write
your new value.

## Weird

- I have some notes from Cliff's talk about what `get` should do when
  copying. They seem to diverge from what I understand via ChatGPT.
- My old notes suggested Click says:
  - Read from old table, write value primed in new table.
  - Go back to old table, try to prime?
  - Only on success in new table do you rewrite as primed?
- But I don't understand how that would work? Or why he would do that?
- I explicitly suggested _not_ doing that, and instead doing it the wayw
  ChatGPT suggested in 2026-04-XX:
  - Prime old version to say don't update anymore.
  - Copy value from old to new, but only writing value if new table
    stores `null` (let new writes win).
- Maybe I understood Cliff wrong in one of the lectures? I would have to
  rewatch his video?

# Copy Helpers

- So we see that `get`/`set` operations will do some copying work.
- Threads that `get` or `set` attempt to do a block worth of copying.
  - Basically, a thread will try to inform others that it has started
    copying a block by updating `_copyIdx`.
  - Only point is not for _everyone_ to try copying same block.
  - But threads won't wait forever, and will eventually start trying to
    copy blocks that others may have already started.
  - This is necessary to preserve liveness.
- We track how many items are copied with `_copyDone`, which is CAS'd.
  When `_copyDone` reaches the size of the old store, we can "promote"
  the new store.
  - We will update `_copyDone` by one when copying over a single value.
  - We will update `_copyDone` by the number of copies done in a block,
    at the end of the block.
  - The check of `_copyDone` and promotion is done in
    `copy_check_and_promote`.
- One minor flaw:
  - A thread can do a bunch of copying but then be _slept_.
  - It doesn't update `_copyDone`.
  - In that case, `_copyDone` never attains the old store size. So we
    never release the old store.
  - Note: `_copyDone` is only incremented _once_ per cell. So no one can
    "redo" work already completed by another thread.
    - See `copy_slot` to see he is careful about that.
    - It is important that two threads not think they've completed the
      same job, else `_copyDone` would be incremented twice for one
      cell.
  - The flaw is that release of the old store can be arbitrarily delayed
    by thread suspension. But at least it won't block further use of the
    map.

## Resources

- https://www.youtube.com/watch?v=HJ-719EGIts
- https://www.youtube.com/watch?v=WYXgtXWejRM
- http://web.stanford.edu/class/ee380/Abstracts/070221_LockFreeHash.pdf
- ChatGPT discussion in 2026-04-XX.
