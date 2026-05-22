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

Each core will have its own cache. The cache stores "lines" (typically
64bytes in length), which are blocks of words. They will share a
bus/connection with a cache controller.

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

# Cache Associativity

Caches often have _associativity_. A **fully associative** cache means
that any cacheline can be replaced on a miss. **Direct-mapped** means
exactly one line could be replaced. `k`-way associativity means that any
of `k` lines might be replaced. This tends to be the default in CPU
caches.

Basically, you have a limited number of cache line slots; if you can
only place a cacheline in one slot, then this is direct-mapped. You must
eject whatever was in that line.

If you have associativity, you need to (1) decide which line to evict,
and (2) you have more places to look when you must read a line.

The advantage is that you don't have to evict the hottest line just
because it is located in the spot you need to place this cache line.

If you have a choice, use a policy such as ejecting the least recently
used cacheline in the set of `k` choices. In practice, a variety of
simpler policies get used, because LRU tracking is kind of
complicated/expensive.

Basically, associativity will increase the hit rate, at the expense of
storage/retrieval time. How much will it increase hit rate? Depends on
how well you can select the line to evict. Also, it depends on the
distribution of "hotness" to the lines in the cache. If all the cache
lines are about equally hot, maybe choosing randomly is a fine, cheap
policy. But since every read/write requires loading a line into cache,
the hotness might really vary.

# Cache Coherence Strategies: Snooping and Directory-Based

## Snooping

Snooping is the alternative strategy. You broadcast coherence messages
on a shared bus, and every core looks at every message to see which are
relevant.

Snooping doesn't scale well, because every core needs to review every
coherence request to see if it is relevant. Also, the interconnect can
become saturated (run out of bandwidth), and even the unsaturated
latency may increase, especially if users are "distant" from each other
(for instance, in different NUMA domains).

Snooping is typically more performant (until scalability limit), and is
the default for SMP systems.

## Directory-Based

The alternative idea is called "directory-based". Here, there is a
central "directory" that knows who has a cache line and in what state
(who are the owners and sharers, for instance). A core that wants to do
something with the line will contact these parties directly.

There can be more than one directory. This is typical in NUMA systems;
you'll have a directory per NUMA domain (normally this is per-socket).
That way the memory controller/directory sits near the memory that it
manages.

Communication between actors in this system is more like point-to-point
rather than broadcast on a bus.

You normally go directory-based when you start to scale past SMP limits
and need to do NUMA.

Note: a lot of systems are hybrids. Within a NUMA domain, you might do
snooping, but across domains you might use directories.

# Cache Writeback

Cache coherency takes care of visibility of writes across CPUs. You
don't need to explicitly force a cacheline to be written back to memory.

But sometimes you really do need some memory to get written *now*, and
not just cached. The main example is for an update that needs to be
persisted. For instance, if we want to write to a disk, we need to put
the cacheline into memory where the disk controller can see it.

It's a bit like making a call to sync buffered IO.

We typically use `CLWB`, which syncs the cache line to the memory (but
keeps holding the line exclusive). We can also use `CLFLUSH` which syncs
and evicts the line, which might be helpful if we know we're done with
the line. `CLFLUSH` is also used in some technical situations.
