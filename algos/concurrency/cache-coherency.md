# Cache Coherency: What Is It?

Since memory latency is poor, it's typically for CPUs to have big
caches, that's another part of the question. How do we keep those caches
"coherent"? That is, if CPU1 writes a memory address, CPU2 should see
this write (eventually). This is _cache coherency_.

More: writes from a core to a (single) memory address should be seen by
other cores in the same order they were issued. If one core issues
`x = 3; x = 5`, then another core must not see `x == 5; x == 3`. The
ordering of writes does matter, but cache coherency is more about making
sure everyone can see the writes more than the order.

BTW: when one unit writes a single word value to memory, another
execution unit cannot observe a "partial" update of the word. It either
sees all bytes of the word updated, or none. Thus single word
read/writes are "atomic."

**Memory Re-Ordering**

Beyond cache coherency is "memory ordering." This is about updates to
distinct words. If one core updates W1 and then W2, then a different
core might read the new value of W2, but then read the *old* value of
W1. Some architectures prohibit certain kinds of instruction
"reordering", but generally at least *some* reorderings are allowed for
optimization reasons.

See `memory-ordering.md` for more discussion.

# MSI, MESI, MOESI Protocols

Each core will have its own cache. The cache stores "lines", which are
blocks of words. They will share a bus/connection with a cache
controller.

## MSI

Let's first consider **MSI**. In a cache, each line will be in one of
several states:

- Invalid: means the cache line is not loaded. You can't use whatever
  value might be stored here.
- Shared: the cache line is loaded and valid in this cache, and it might
  also be loaded in other caches. Since the line is unmodified, we call
  it "clean".
- Modified: the cache line has been modified in this cache. It is
  required to be invalid in every other cache. We also call this
  "dirty".

**INVALID to SHARED**

When a core tries to read a line, it first checks to see if it is marked
`SHARED` or `MODIFIED`. If so, it can use the local cached value without
communication with the controller. If it is `INVALID`, it must ask the
controller for the value. When it receives the value, it puts it in the
cache and marks it `SHARED`.

Note: the controller will know if any cache has the line marked
`MODIFIED`. In that case, it will order the "owner" of the `MODIFIED`
value to write the value back to the cache controller. This core will
transition their cache copy to `SHARED`.

**SHARED to MODIFIED**

When a core tries to *write* a line, it first checks if the line is
`MODIFIED`. If it is, the core can update the cache line without
communication with the controller.

If the line is `INVALID` or `SHARED`, the core first asks the controller
to tell order other cores to transition their copies to `INVALID`. When
this is done, the local core transitions its line to `MODIFIED` and
writes the value into it.

## MESI

MESI adds an `EXCLUSIVE` state. When a line is read by a core, the
controller will tell the core if it is the *only* owner of the line. If
so, it marks the line `EXCLUSIVE` instead of `SHARED`. This is a
*second* "clean" state. The controller will also need to record that the
line is stored `EXCLUSIVE`.

The advantage comes when a core writes to a line marked `EXCLUSIVE`. In
this case, we do not need to ask the cache controller to invalidate the
other caches. We simply upgrade locally, because we know we're the only
holder of this line.

The downside comes when coreA tries to read a line from the controller,
but the controller has recorded that coreB has the line marked
`EXCLUSIVE`. The controller doesn't know if coreB still has the line
`EXCLUSIVE` or if it has even written to the line and (independently)
upgraded it to `MODIFIED`. Either way, the controller must tell coreB to
downgrade the line to `SHARED`, possibly even writing back the value to
the controller.

Note: if you use "snooping" (where cores share a communication bus and
can listen to each other's messages), this may often not require another
round of communication. coreB will signal a `SHARED` bit (and downgrade
to `SHARED`), so that coreA knows to mark its copy `SHARED`. If no one
signals `SHARED` on the bus, then the reader marks the read line as
`EXCLUSIVE`. But if coreB really did write the line, it must communicate
that back to the controller.

When does `EXCLUSIVE` help? Especially when there is a lot of *thread
local state*. In that case, a lot of lines will be read and modified
only by a single core. Writes on thread local data do not require an
extra communication with the controller. That happens a lot so is a big
win.

The main downside to adding `EXCLUSIVE` is if coreA reads a line, gets
it exclusive, but *doesn't* write it. Then, coreB reads the line, so
coreA must downgrade to `SHARED`. If you use snooping, then this doesn't
involve an extra round of communication. But if you require all
signals/messages to pass through the cache controller, then the
controller needs an extra round to tell coreA to do the downgrade.

Practically, the only downside is increased implementation complexity.
And, the assumption that most state is thread local is usually correct.

## MOESI

You add an `OWNED` state. This is a second *dirty* state.

The basic idea is that if coreA has a line marked `MODIFIED`, and coreB
does a read, then coreA can transition to `OWNED`, and provide the data
directly to coreB. That is: when a line is marked `MODIFIED` or `OWNED`,
the core must snoop all requests to read the line and provide the data.

A core that marks a line `OWNED` is not allowed to further update this
line without telling all other cores to mark their copies `INVALID`.

This helps when the bottleneck is the controller's limited bandwidth to
write back lines. It assumes that communication bandwidth between cores
is not the bottleneck.

The good case is when coreA is the only modifier of a line, and others
want to read it occasionally. When coreA modifies the line often, then
unnecessary writebacks are saved. But if coreA will never modify the
line again, then it might as well just write it back to the controller
when another core asks to read it.

Is there any downside to this extension of MESI? First, complexity.
Second, if all communication must route through controller, than a
request to read when a line is `MODIFIED` or `OWNED` must go through a
longer communication path:

1. coreB requests from controller,
2. Controller requests from coreA,
3. coreA marks as `OWNED` (if `MODIFIED`), and supplies line to
   controller,
4. Controller responds with line to coreB.

If coreA can snoop the bus, then you avoid the extra rounds. There is
not more messages being transmitted. However, you have to change the bus
communication to allow for snooping, and for *anyone* to reply to a
request (rather than the controller always being the respondent). Also:
every core has to snoop to every request, and check whether it has
marked that cache line `OWNED` or `MODIFIED`. These are subtle costs,
but they exist.

When there are a small number of cores on a shared bus, `OWNED` is often
worth it. And the "worst" case is very odd and seldom occurs. But if
there are many cores, and if the cores are not all on the same bus and
instead go through multi-hop routed communication, then the `OWNED`
state starts to cost more than it is worth.

## MESIF

Here, `F` is for `FORWARD`. It is another *clean* state. Like `OWNED`, a
core that has a line marked `FORWARD` will respond to requests to read
the line. Except here the line is clean.

Each time a line previously marked `INVALID` is read, the core marks the
loaded line `FORWARD`. Only at most one core at a time will mark a line
`FORWARD`. If the read comes not from the controller but another core,
than the supplier downgrades the line to `SHARED`.

`FORWARD` can make sense when read bandwidth to the shared memory via
the controller is the bottleneck. `FORWARD` presumably makes most sense
when cache-to-cache communication cost is low (plenty of bandwidth).

The controller may not always know if a line is marked `FORWARD`. For
instance, the `FORWARD` line might be ejected from the core's cache for
lack of interest. When the controller thinks someone might have the line
`FORWARD`, it will wait to satisfy the request. But if no one responds,
the controller should consult the memory and respond. That can add
latency.

## MOESI vs MESIF

MOESI reduces writeback of dirty data. MESIF allows distributing
read-load on clean data.

You can combine MOESI and MESIF. Real cache coherency solutions are less
clean than these textbook ideals, and ideas can be mixed.

If you had to choose one, maybe MESIF is better because clean shared
data is more common than dirty shared data. MESIF optimizes read-miss
response to a clean value; a common scenario for code pages, read-only
data, configuration, read-mostly structures. Read-miss latency matters a
lot because a core needs that to progress.

Whereas MOESI is more about reducing write traffic to the controller.
Assuming you can snoop and avoid controller, you might reduce read
latency for read-miss when the data is held dirty in another cache. But
if most traffic to controller is *read* traffic, it can make more sense
to distribute *that*, rather than reduce write traffic.

Writebacks can often be buffered by the controller, which allows the
program to proceed so long as the write buffer is not full. Whereas
readback from the shared hierarchy is critical for program progress and
cannot be buffered. As long as the write buffer is not filling,
optimizing for fewer writebacks might not actually help performance.

AMD x86 design has traditionally leaned MOESI and Intel x86 design has
traditionally leaned MESIF.

# TODO

**TODO**: I think write buffers should mention that a lot of the time
you need a special instruction to force update to be pushed to RAM. To
flush from cache.

## Directory-Based Cache Coherence

In NUMA systems you typically use this cache coherency strategy. Each
CPU has its own directory. It keeps track of who has each
cacheline. All communication is done point-to-point.

Slower than snooping when there's enough bandwidth, but scales better.

I should find a better resource, I don't actually know 100% how this
works.

## Associativity

Caches often have _associativity_. A _fully associative_ means that
any cacheline can be replaced on a miss. Direct-mapped means exactly
one line could be replaced. `k`-way associativity means that any of
`k` lines might be replaced. Basically, you have a limited number of
cache line slots; if you can only place a cacheline in one slot, then
this is direct-mapped.

If you have associativity, you need to check more places for a
cacheline. But this way you don't necessarily have to replace a hot
cacheline just because of a collision. Remember that collisions can be
quite common, when the cache is small, and the hot set is of the same
order of magnitude as the cache size.

Basically, associativity will increase the hit rate, at the expense of
retrieval time.

## Cache Coherency and TAS vs TTAS

Say you want to spin, waiting for a value to be set. _Local spinning_
occurs when you have the value in cache, and you just keep checking
until it is invalidated. In that case, you do not need to talk to the
bus, and you should not interfere with the other processors. This is
the ideal version of spinning.

When you do a TAS, it needs to talk to the bus. These failed requests
can saturate the bus, destroying performance. With TTAS, you do not
make bus requests that you _know_ can't succeed.

**How is TAS implemented?**

So X86 has a LOCK instruction _prefix_, and you can use it on an
instruction like INC (increments one value), or XCHG (swaps two
values), or CMPXCHG (basically compare and set). These instructions
are probably almost meaningless without the LOCK prefix.

It appears that to accomplish this, the machine must take exclusive
access of the the cacheline (for CMPXCHG, maybe two cachelines?)
involved.

In Intel 486 days, apparently they did an entire lock on the memory
bus. Starting with Pentium Pro they do a cache lock on just that line.

- Resources
- SO on Lock Prefix: https://stackoverflow.com/questions/8891067/what-does-the-lock-instruction-mean-in-x86-assembly
- Helpful Quora Post: https://www.quora.com/How-is-the-LOCK-instruction-implemented-in-the-Intel-processors
  - Actually just excerpts https://software.intel.com/en-us/articles/implementing-scalable-atomic-locks-for-multi-core-intel-em64t-and-ia32-architectures
- **Wait this is the real deal:**
  - http://davidad.github.io/blog/2014/03/23/concurrency-primitives-in-intel-64-assembly/
  - It sounds like basically doing a LOCK operation necessarily
    means bus traffic to tell everyone to give us exclusive access
    (and further to lock them out from future access).
  - We can avoid this if we have _shared_ access already, and then
    we can just test locally and see the lock cannot be acquired.
  - When a lock is freed, our local copy will be invalidated, and
    _then_ we can acquire the lock.
- These slides explain the same thing:
  - http://www.cse.iitm.ac.in/~chester/courses/15o_os/slides/9_Synchronization.pdf

**More About Cache Lines**

They make a related suggestion: try to keep a contested lock and its
data on different cachelines, so that attempts to get the lock won't
invalidate the data (and changes to the data don't signal that the
lock might be free).

Actually, it looks like maybe CAS isn't ridiculously expensive; just a
couple cache misses. First you normally have to read the original
value; that could be a cache miss. Then you have to "lock" the
value. To do this, in MESI cache coherency, you tell people that
you're taking this, and they have to invalidate. That is a 2nd cache
miss (unless you already had exclusive access!). Now you have
exclusive access and can modify this in your cache.

Source: Art of Multiprocessor Programming
Source: http://stackoverflow.com/questions/2538070/atomic-operation-cost

## Memory Consistency

**TODO**: Maybe integrate to `memory-ordering.md`.

Processors try to keep all their hardware units busy, by doing
out-of-order execution and work in parallel.

Write requests to RAM are typically buffered. This hides latency,
utilizes bandwidth better through batching, and eliminates some
requests due to _absorbtion_ (when several writes can be
consolidated).

Consider a mutex algorithm _without_ hardware support. It's very
important that everyone have a consistency view of memory. We know
that without cache coherency, we could write a variable on one thread,
and then do a read in a second thread, not seeing the write.

Cache coherency solves the problem introduced by caches. But _there is
still a problem_, introduced by the write buffer (and presumably from
OOE). This is why we have memory fences; someone who issues a fence is
guaranteed to see all other writes from the other processors before
that fence was issued. Fences can take 100s of cycles.

They contrast CAS (which has the ABA problem: update from A to B, then
from B to A, and now they can't see the change). Another possibility
is `load-linked` and `store-conditional` (from PowerPC), where the
`SC` works only if the variable hasn't changed since the `LL`. The
only problem here is you have to be careful what you do between the
`LL` and the `SC`...

Random note: parallelism on a single CPU with multiple cores can be
advantageous because of the shared L2 cache (L1 typically not shared).

Source: Art of Multiprocessor Programming

## WTF Write (AKA store) Buffers?

I think the idea is this: for OOE to reorder memory accesses, it uses
a store buffer. A store is not actually performed until the processor
knows that it doesn't violate a dependency. But the processor cannot
know about dependencies across cores or processors.

When I say "memory access" or "memory write", we really mean L1
cache. That's why this is a problem even when we have cache coherence.

Source: https://en.wikipedia.org/wiki/Memory_disambiguation
