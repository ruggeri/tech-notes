Wants to build a lockfree hash table. Tries to be really practical,
especially for many many CPU. Uses a bunch of cache friendly
techniques.

He uses open addressing, with a reprobe of 1 (for cache
friendliness). The number of buckets is a power of two for fast MOD
(since it's just an AND). He puts the `(key, hash, value)` all in the
same cache line (the hash value allows faster equality check). There
are no allocations in a put.

When a key is set at a location, it never is deleted or changed. That
will be essential for correctness: this way you never get a value for
the wrong key. Values are either NULL, set, or TOMBSTONE.

Get: lookup, check the key. If it's not there, return NULL, if it is,
return the value (if any is set yet). If the wrong key is there, start
walking and repeat. NB: if memory order is weird and you see the value
before the key, that's fine, treat it as a miss.

Set: do a CAS on the key, then the hash, then the value. If anyone
sees an in between state, no problem. If there's an old value there,
you'll just change that. Delete works likewise, setting the
TOMBSTONE. Optimization: if your CAS of the value fails, then just
stop, because you can treat this as someone overwrote your value!

Again: I want to note that there is no need for fencing at this
time. If other procs see this information out of order, who cares?
Coherency/coordination is on the user: they can use as much as they
want.

To be clear, not all processors will see the same HM; there's very
little coordination. The only thing that can't happen is stomping on
each other's keys, since that's a CAS. I don't think you'd even need
to do CAS for the value (since uncoordinate sets can stomp each
other), except that later we'll need it for the resize (I think)...

## Volatile Values?

Okay, he talks about making the value `volatile`. This means the
compiler doesn't try to cache the value in a register and will instead
try to read it from memory. That's needed because other threads can
modify that variable, and the program shouldn't assume that it hasn't
changed.

Note that the CPU might still cache the value. In that case, we
normally rely on cache coherency to keep that cached value up to date
when other CPUs modify the value.

I'm not sure I've understood what he's talking about here. It feels
like he needs `volatile` so that other threads can see the updates,
but I don't understand if he means anything more than that?

## Resizing

Resize when you're too full, or when you get too many
collisions. That's a good strategy, in case the user's hash function
sucks. Also, if you resize on reprobings, you don't need to coordinate
any count variable.

Resizing copies KV pairs, and removes tombstoned KVs.

Get: you return the value from the old table, unless you see a
sentinel (which means the key has been relocated).

Set: will use the new table. It then marks the old table value as having
moved.

Some setters will not know yet about the new table. What if I do a set
in the new table, and mark the old table? Then a setter in the old
table will either (1) see this, and retry in the new table or (2) fail
the CAS, then retry.

Note that I can blindly write the sentinel in the old table, since no
writes to the old table can "happen after" this current set! Click
explicitly says that you can stomp the old table in this case.

**Careful!**

What about copying old values *without doing a write*? For instance,
say an old thread wrote a key. Now I want to copy that key to the new
table. I am *not* writing the key, just copying.

I could read the value, write it in the new table, and then set the
sentinel in the old table. But what if a slow writer just got around
to writing an update in the old table? I can't lose that update! Click
explicitly says this is the problem the prime is supposed to solve.

Click suggests this:

* Read from the old table.
* Write the old table's value into the new table, but primed, saying
  it might not be the last version from the old table.
* Go back to the old table, try to set the sentinel.
    * If you fail, no worries. There are more updates coming in.
* If you succeed, now mark the value as *not* primed, saying you don't
  need to go to the old table anymore. This is for efficiency, obvs.

## My Alternative?

I would have thought you do this:

* Set the old version primed, saying "please don't update me anymore!"
* Then copy the value into the new table, only overwriting if the old
  value is null (i.e., let new writes win).
* You don't even need a tombstone anymore, because you're telling
  people to come to the new table. Maybe?

I can't understand why my way wouldn't be preferable? Isn't mine
easier? Maybe for some careful reason regarding fencing?

## Resources

* https://www.youtube.com/watch?v=HJ-719EGIts
* https://www.youtube.com/watch?v=WYXgtXWejRM
* http://web.stanford.edu/class/ee380/Abstracts/070221_LockFreeHash.pdf
