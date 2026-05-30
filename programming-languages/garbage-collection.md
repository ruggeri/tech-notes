# Concept: Manual Memory Management and Garbage Collection

Any object that will not be used again in the life of the program is
eligible for reclamation. However, it's generally not *decidable* what
objects will be used again.

But we can easily identify many objects that we are sure will not be
used again. These are the objects that are not referenced (even
transitively) from the set of objects on the stack.

"Garbage collection" is when the programmer does not explicitly handle
object reclamation. Instead, the runtime itself attempts to identify
garbage and reclaims it. The opposite is manual memory management, where
the programmer is responsible for the identification of objects that
need to be reclaimed. Garbage collection is a form of automatic memory
management.

In C, you manually reclaim objects with `free`. In C++, you often rely
on a *destructor* which runs when an object on the stack falls out of
scope. Destructors also run on fields when their parent object is itself
destructed.

With manual memory management, you are responsible for freeing objects
no sooner than the time of their final use by the program. You can free
objects at any time thereafter. Memory that you can but fail to free is
called a *memory leak*.

With garbage collection, you cannot always know at what time an object
will be freed. GC guarantees not to collect an object earlier than is
safe, but typically does not guarantee how "late" the collection will
take place.

## Finalization

I just want to note a related concern, which is *finalization*.
Finalizers are code that runs when an object is garbage collected. But
*when* will the object be collected? In Java, you can be sure of
finalization only with a `try-with-resources` block:

```java
try (FileInputStream s = new FileInputStream("x")) {
    // use s
} // s.close() is called here deterministically
```

Similar concepts are:

- Python: `with open("data.txt") as f:`
- Ruby uses blocks: `File.open("data.txt") { |f| ... }`
- JavaScript/TypeScript uses `finally`: `File.open("data.txt")`

Go is odd, it asks you to use `defer` to add a method to be run at end
of lexical scope.

```go
f, err := os.Open("data.txt")
if err != nil {
    return err
}
defer f.Close()

// use f
```

In C++, which does not have GC, it is more typical to use the RAII
concept, and rely on the destructor running at the end of lexical scope:

```C++
void read_file() {
  File f("data.txt");   // acquire in constructor

  //  Do stuff

  // At end, File object gets destructed and destructor closes file
  // handle.
}
```

# Reference Counting

With reference counting, each heap-allocated object stores a reference
count. Each time you store a new reference, you increment the count.
Each time you remove a reference, you decrement the count.

Whoever decrements the count to zero must destroy the object.

The main problems with reference counting are:

- The count bloats every object. Probably not a massive cost since
  object overhead must always exist.
- Every mutation of a pointer needs to decrement one reference count and
  increment another. This can increase memory bandwidth usage.
  - Can cause unpleasant cache contention when multithreading. Storing a
    reference to an object should not otherwise need the object
    `EXCLUSIVE`, but you have to gain the object `EXCLUSIVE` if the
    count is in the same line as the data...
- Potentially unbounded pauses when you're destroying objects.
  - This can be addressed by incrementally decrementing reference
    counts. When an object's refcount falls to zero, put it in a queue,
    and another thread can then go on decrementing the items referenced
    by the object.
- Need locking or atomic operations in multi-threaded programs, hurting
  performance.
  - This sort-of repeats the earlier point...
- **Cycles are not collected.**
  - This can be addressed if you very occasionally run a tracing
    garbage collector. This is what Python does.

CPython is the traditional example of a language/runtime that uses
reference counting. It also uses a cycle detector. PyPy, interestingly,
uses tracing GC.

The advantages are:

- Simple.
- Can reclaim items as soon as they lose all references.
- Tracing GC can perform poorly if you have huge heaps with lots of live
  objects, because you must trace the entire working set.
  - The benefit/cost ratio plummets if most objects in a very large heap
    are live.
  - Also, walking the heap objects tends to page all memory in.

# Tracing Garbage Collection

Basic idea is that you will "trace" from the root set, following all
references, and marking all the objects that you visit. At the end of
the trace, any objects that were not marked are available for
collection, and their memory can be reused.

## Pointer Arithmetic: Precise/Conservative

Tracing really only works if pointer arithmetic is not allowed, or is
very limited. When there is no pointer arithmetic, a GC knows exactly
what machine words are pointers (this is called *precise*).

A *conservative* GC must assume that *any* word *could be* an address.
This basically only applies to garbage collectors bolted onto C/C++.
People do try that kind of thing, but it is niche and a bit odd.

Precise GC will need to be able to find the references inside heap
objects. A simple (but not entirely free) way to do this is to store
some class metadata at every object. This does involve some object
bloat.

## Attributes: STW/Incremental/Concurrent/Parallel

We will start with a survey of basic approaches. Later we will consider:

- Whether GC must **stop the world**, prohibiting the program from
  running while the GC runs.
- Whether the GC can at least run **incrementally**. An incremental GC
  might stop the program, but yield back to it after doing some
  tracing/sweeping. The program eventually might yield back to the GC
  and progress continues.
  - In general, incremental collection requires more synchronization
    between mutator and collector than collection that stops-the-world
    for the entire collection. We expect to trade reduced aggregate
    throughput for shorter GC pauses.
  - There is a spectrum between STW collection and incremental
    collection. For instance, we might trace incrementally, but still
    STW for the entire reclamation/disposal/live object copying.
- Truly **concurrent** GC can run garbage collection thread(s) in
  parallel with the program.
  - Concurrent GC allows for preemption of GC thread to run the program.
    It also allows *parallel* execution of the program and collector.
  - Again, a collector concurrency is a spectrum: maybe the trace can be
    done concurrently but space reclamation must STW.
- **Parallel** GC runs *multiple* garbage collection threads
  simultaneously.
  - Parallel GC might still stop the world and not be "incremental".
    Kinda weird naming!
  - We expect that on a single core machine, or when there is little
    garbage collection work per thread, that parallel collection might
    not be faster than serial collection.

As discussed, tracing GC needs to walk the object graph, which pages in
all the memory. We will later see a few ways to reduce graph walking to
a subset of most valuable areas to trace (generational and region
concepts).

## Mark-and-Sweep Collection

In mark-and-sweep collection, you do your usual trace.

When done tracing, you then iterate an *object list*. Every object that
is not marked can be placed on the *free list*.

A first problem is *fragmentation*. If you have a lot of "small holes"
of free space, you may not be able to satisfy a large allocation.

Second, when making an allocation, how will you find a large-enough free
chunk in the free list? You could try to sort the free-list by chunk
size. Or you could have multiple lists for various common chunk sizes.
But unless you sort by *chunk start address*, it can be hard to
*coalesce* consecutive smaller chunks into a larger chunk.

## Semi-space Copying Collection (Cheney Collection)

You have two spaces: a `fromspace` and a `tospace`. Memory is allocated
in `fromspace` until full. To allocate, you just bump a pointer to the
end of the used portion of `fromspace`.

When `fromspace` is full, you must now "evacuate" `fromspace` and copy
it to `tospace`. The entire `fromspace` is reclaimed. The role of
`fromspace`/`tospace` reverses.

Cheney's algorithm is to:

1. Copy the initial objects referenced by the stack (also registers,
   globals, et cetera...) into the `tospace`. Do not yet modify any
   pointers contained within these objects.
2. At the old `fromspace` location of the object, leave a *forwarding
   pointer*, which is the location in `tospace` where the object now
   lives.
3. Now, use the `tospace` as a queue. Iterate the `tospace`.
4. For each object in `tospace`, iterate the pointers within the object.
5. The pointer points into `fromspace`. Check if the referenced object
   has already been moved. If so, there is a forwarding pointer left
   there. Update the reference.
6. Else, the object has *not yet* been moved. So move it to the end of
   `tospace`, and leave the forwarding pointer. And of course update the
   reference.

This copies all live objects from `fromspace` to `tospace`. It uses the
`tospace` as a queue to do BFS within.

Note: you do not need separate tracing and copying phases.

Because space is "compacted" as objects are copied, you can allocate
very quickly without consulting a free list. Also, higher density of
live data *might* increase locality of reference (especially relative to
a bunch of dead space in between objects). On the other hand, the layout
that results from a BFS trace is not necessarily optimal.

When Cheney collection starts, you must allocate a `tospace` large
enough to hold all live objects. If almost all the `fromspace` may
contain live objects, you really must allocate 100% of "extra" memory.
You cannot re-use any `fromspace` until the collection completes,
because that is where forwarding pointers live.

## Mark-Compact

Mark-Compact first runs a tracing phase. But, instead of putting free
chunks on a free list, Mark-Compact "compacts" objects by "sliding" them
together, eliminating the free space in between them. This might improve
locality of reference even better than Cheney collection, but results
are variable and depend on access pattern. And it will allow for fast
allocation. And it will not use two spaces.

We'll iterate through live objects in address order. We'll slice each
object to the end of the live space. After sliding everyone together, we
will have to iterate references to the space to update with the new
location. A classic "textbook" way to do that is a "break table",
discussed next. But there are many ways to do it, and break tables
aren't necessarily the default in modern mark-compact implementations.

### Break tables

As we slide objects together, whenever we encounter free space, we will
make a note in a "break table". The break table basically maps an
address to a "delta". Objects that live after the address have been
shifted "down" by the specified delta. After everything has been slid
down, we re-iterate the live objects, and update all the reference
addresses. This is quick to do by reference to a sorted break table: we
do a binary search for the relevant delta and adjust accordingly.

An alternative would be to place "forwarding pointers" at the old
locations. But this isn't trivial if we are not "evacuating" the
collected space to a free space. If we are sliding objects within the
same "region", then eventually some of the slid live objects will
overrun the old locations where forwarding pointers need to be
preserved.

But that begs the question: where does the break table live? You want to
avoid pre-allocation of a potentially large table. Ideally the table
will live in the free space reclaimed by the collector.

Basically: you iterate until you find the first "free" space. You start
the table here; you put in your first entry (entries must be small
enough to live in any reclaimed space). You continue sliding down
objects. But sliding an object will overwrite the first entry in the
table. So move this entry to the *end* of the table. This is a kind of
swap:

1. Copy first "line" of table to a temporary location.
2. Move (at most) line worth of live data down. This overwrites at most
   one line of the table.
3. Bump/increment the pointer to start of table.
3. Place the old first table line to the end of table.
  - This might overwrite space of old object. That's okay, we moved it!

This is called "rolling" the table, and it disorganizes the table. The
benefit is that we do not need to allocate any space to hold the table;
it lives within the memory we have reclaimed! So, after compaction, we
sort the break table. That is `O(n log n)`.

# Tri-Color Marking: Toward Incremental Tracing

We will begin exploring how to trace *incrementally*. This will involve
alternating the program (the "mutator") with the collector. The mutator
will mutate references during in between collection periods: that's the
new challenge!

To help guide us, we will define three sets:

- White objects. Any object that remains white at the end of tracing
  should be collected.
- Gray objects. We must examine the gray objects. Tracing is complete
  when the set of gray objects is empty.
- Black objects. At the end of tracing, any objects colored black will
  be retained. During tracing, black objects do not need to be
  re-examined.

You start out with the root set as gray. You iterate gray objects,
examining their references. If the object referred to is white, you
color it gray (and put it on the gray list). If it is gray or black,
ignore the item. After scanning all outgoing references of a gray
object, you can color it black.

You can continue until everything is black or white; now you know the
white stuff can be thrown away. So far this is just the typical
tracing. Objects move from white to gray to black.

However, our next step will be to allow reference mutations from the
program that occur concurrent to an incremental collector. Else the
program must wait for all garbage collection to complete before it can
resume running again. But modifications from the program will have to
change an object's color in order to preserve correctness...

## Incremental Update (Dijkstra Barrier)

(At this point we are not yet allowing the program and collector to run
simultaneously. We only allow them to be interleaved. Else we have to be
even more careful about synchronization.)

Consider a mutation to a reference stored in `objA`. This (1) removes a
reference to `objB` and (2) adds a reference to `objC`.

If `objA` is white or gray, then we don't have to do anything.  `objA`
has not yet been traced, but when it is, we will see the reference to
`objC`. In that case, `objC` will be grayed, and retained. (Be careful:
if a gray object can be "partially" scanned when the mutator resumes,
it's not safe to assume that we can assign `objC` into a gray `objA`
obliviously.)

The trouble is if `objA` is black. Then we have two problems. The first
is minor: we've removed a reference to `objB`, which has already been
grayed. If this is the last reference to `objB`, then `objB` is eligible
for collection, yet we've already grayed it. We will fail to collect it
in this GC round. However, it will certainly be identified as garbage in
the next GC round. It is not an error to fail to collect `objB` in this
GC cycle, but it is not desirable to miss it. The possibility that
`objB` doesn't get collected when it could have is practically
unavoidable: it comes from interleaving mutations with tracing.

But what about `objC`? We are creating a reference to it, which means we
must retain it if the reference survives to the end of the cycle. We can
be sure of this if `objC` is already black or gray. The trouble is if
`objC` is white.

What we see is that the runtime must run some code for every reference
assignment. This is called a *write barrier*. This has little-to-nothing
to do with memory barriers/fencing from multiprocessor computing. It's
just a name for the function we run on reference assignment. We cannot
allow "raw" reference assignment. This kind of write barrier is also
called a *Dijkstra barrier* after Dijkstra:

```
# "Precise" Dijkstra Barrier
write_barrier(objA, field_name, objC):
    if marking_is_active
       and is_black(objA)
       and is_white(objC):
           shade_gray(objC)

    objA.set_field(field_name, objC)
```

This ensures that `objC` will be retained.

### Optimization: Conservative Dijkstra Barrier

It is usually cheap to test `is_white`, since this is normally reflected
as a marker bit in the object header (or a side bitmap) being zero. But
distinguishing black and gray can be more expensive. In typical
implementations, black and gray objects both have the marker bit set.
Often what makes an object gray (rather than black) is its presence in a
workqueue of objects for the tracer to examine. It typically isn't
cheap/easy to test for inclusion in the queue.

Thus, a "conservative" optimization that shades more objects (and
potentially "floats" more garbage), is:

```
# "Conservative" Dijkstra Barrier
write_barrier(objA, field_name, objC):
    if marking_is_active
       and !is_white(objA)
       and is_white(objC):
           shade_gray(objC)

    objA.set_field(field_name, objC)
```

This shades `objC` even if `objA` is merely gray. This means that `objC`
will not be collected even if all references to `objC` are removed
before `objA` is examined. This optimization introduces another pathway
by which garbage can fail to be collected this cycle ("float").

It would also be valid to regray the *parent* `objA` instead of the
child `objC`. This involves re-examining all the fields of `objA`, which
tends to be more expensive. There's also a danger that GC cannot ever
complete if we keep transitioning objects from black to gray. But this
strategy could float less garbage.

### Final Root Set Re-Scan

At the start of tracing, we gray everything on the stack (and in
registers and globals et cetera). That is our initial root set.

We talked about how we gray objects as references are stored at black
objects.

But what about changes to the *root set*? At the end of tracing, we
cannot collect an object if it is still referred to from the stack or
other root set location.

It's one thing to gray references stored in the heap. But we may
especially not want to gray every object that gets stored onto the stack
as soon as it is stored. For instance: stack frames are constantly
popping so most of these references will be lost soon. Most objects can
usually only survive long-term if they are stored in the heap.

The typical solution is this: "gray the stack/root set" at tracing
start. This is our "root set" to trace from. Now, trace fully,
incrementally. Last, *stop the world*. Examine the stack/root set and
gray any referenced white objects. Trace, and drain the gray queues.
Hopefully this stop is brief if there isn't a lot of as-yet-untraced
data that can be reached from the stack.

This last stack re-scan is sort of like if we re-marked the "black"
stack as "gray" on modification, instead of marking every object
assigned into a black stack as gray. But we "batch" and "coalesce" the
constant marking of the stack, and only examine it one more time: at the
end of tracing.

## Snapshot-At-The-Beginning (SATB)

The incremental update approach looks for a white child object that is
assigned into a black parent object. It protects against the danger that
this is the only reference to the white object, and that we will fail to
mark the white object.

But how could this be the "only" reference? How could we fail to gray
such the child? First, the white child object be a *new* object, created
as a temporary, with no other references stored anywhere ever. We can
protect against losing the white child by marking *all* new objects
black on creation.

(New objects can be marked black instead of gray if field initialization
happens after black marking, and we use the same technique to protect
against losing white objects assigned into this black parent's fields.)

This has an obvious downside: objects created during the trace are
*always* retained, even if they become garbage by the end of the trace.
With incremental update, this "floating garbage" *can* happen. But with
SATB, we know it *always* happens. Then again, the floating garbage is
surely recovered in the next collection cycle, so floating garbage
should not grow without bound. However, floating garbage does still
bloat total memory use, so it's not a good thing.

The second way to fail to mark a white child assigned into a black
parent is if there *was* a second reference to the white child at the
start of tracing, but that this reference is later removed before it is
traced. The SATB/Yuasa approach is to gray a white object *whenever* a
reference to it is removed:

```
# Precise Yuasa barrier
write_barrier(parent_obj, field_name, new_child):
    old_child = parent_obj.get_field(field_name)

    if marking_is_active and old_child != null and is_white(old_child):
        shade_gray(old_child)

    parent_obj.set_field(field_name, new_child)
```

Notice that Yuasa barrier means that we will not collect anything that
was live on the heap at the beginning of tracing (even it is not live by
trace end). This gives rise to the name **snapshot at the beginning** or
**SATB**.

Because no new objects are ever grayed, we know that SATB will
eventually complete tracing, no matter the rate of creation of new
objects. Contrast with incremental-update, where, if allocation occurs
faster than tracing, the gray set could increase without bound. You have
to be careful of that possibility with incremental update...

Note: you don't have to rescan the stack at trace end because all new
objects are created black anyway.

### SATB Performance: Cache Optimization

Why bother with SATB? We float more garbage with Yuasa than Dijkstra.
Isn't SATB strictly inferior?

A major reason for preferring SATB: the conservative Dijkstra barrier
checks both the parent and child marker bits. The precise Yuasa barrier
checks only the child marker bit. Checking the marker bit may involve a
cache miss. Reference assignment is so common, and it is otherwise so
cheap. Incurring an extra cache miss can therefore be a significant
penalty.

So SATB checks fewer bits (and incur fewer cache misses). Moreover, it
avoids cache misses specifically in the write barrier code, which is
executed in the user program threads. If garbage-collection costs *must*
be incurred, it is best if they are incurred in the GC thread instead of
the program. That's because the GC thread can be scheduled intelligently
(for instance, when program is idle). It also can reduces cache-thrash,
since GC code can make heavier use of the marker bitmap lines and get
more for the cost of loading them.

You might argue: isn't the parent marker bit stored in the object
header, near the parent fields that you intend to modify anyway? Often
no: marker bits are often stored in a side bitmap. This can be more
space efficient (reduces object header bloat). It also means that while
tracing, you can mark the bit without claiming the object data exclusive
and invalidating a mutator's cache lines of program data. And it
increases locality for GC code that must look at marker bits but doesn't
need to examine object fields.

So there is significant benefit to the use of a side bitmap. But the
downside is that if the mutator must read marker bits, it often incurs a
cache miss. With side bitmaps, you can expect a cache miss in the side
bitmap even when the program already has the data cache line exclusive
for a field modification.

### Further Optimization: SATB Queues

For SATB, we often elide the check of `is_white`:

```
# Conservative Yuasa write barrier
write_barrier(parent_obj, field_name, new_child):
    old_child = parent_obj.get_field(field_name)

    if marking_is_active and old_child != null:
        current_thread.satb_buffer.append(old_child)

    parent_obj.set_field(field_name, new_child)

# Run in GC thread
drain_buffer():
    for obj in current_thread.satb_buffer:
        if obj != null and try_mark_if_white(obj):
            current_thread.gray_queue.append(obj)

    current_thread.satb_buffer.clear()
```

Here, the mutator `write_barrier` doesn't even check if `old_child` is
white, it just enqueues `old_child`. The mutator pays the least possible
latency penalty. There is *no* cache miss on marker bits, because you
don't read any!

The GC thread must still check the bits. However, this thread may have
better locality (greater use of marker bitmap lines), increasing
throughput and reducing GC cost. And there is more flexibility about
*when* the cost of GC is incurred. The runtime can try to schedule the
queue drain when the mutator is otherwise idle.

Is this a fundamental advantage of the SATB approach? Couldn't you try
to make the same optimization to the Dijkstra barrier? For instance:

```
write_barrier(objA, field, objC):
    if marking_is_active and objC != null:
           current_thread.buffer.append(objC)

    objA.set_field(field, objC)
```

Yes. This will typically float less garbage than SATB, because you don't
retain old fields that lose all reference. But it might increase logging
if many reference writes are to null fields (typical of initialization).
Almost all items created during a trace will be retained (so long as the
new object is stored in a reference on the heap). Dijkstra also still
has more need of a stack/root set rescan.

### SATB vs Dijkstra summary

The choice of Dijkstra or SATB is practically a technicality. You can
use either system. You can use tricks to improve cache friendliness of
both approaches. As you make the approaches cache friendlier/more
practical, their pros/cons converge almost to a vanishing point.

# Concurrent Tracing vs Concurrent Reclamation

We should be able to pretty easily extend the ideas of incremental
tracing to *concurrent* tracing. Some care may be needed, but the basic
incremental approach should be able to be done concurrently with the
program/mutator. One example: concurrent collectors often need to STW
briefly for root set examination.

It is harder to do concurrent relocation/copying/compaction of marked
objects to reclaim space. Further down, we will explore concurrent
reclamation ideas.

Java introduced the CMS collector in JDK 1.4.1 in 2002. It is a
low-pause collector and does concurrent marking. It also does
*concurrent sweeping*. But note that this collector does not attempt to
relocate object. It sweeps free space onto free lists, which can be done
concurrently easily.

If fragmentation eventually becomes too bad, then it will become harder
to find space to place larger objects. Space will be wasted between live
objects, requiring more frequent collection phases. Eventually, the CMS
collector triggers a compacting collection, which stops the world.

In 2012 the G1 garbage collector became a supported feature of JDK7. But
it *still* does not do concurrent evacuation/relocation of regions of
objects. It stops the world, but for shorter periods.

Only in 2020 did ZGC get supported as a production feature. CMS was
removed at the same time. It offers concurrent relocation of objects. We
will discuss it later. But the 18yr gap between CMS and ZGC demonstrates
how it was much easier to concurrently trace than to relocate objects.

# Generational Garbage Collection

Garbage collection can be expensive. You have to trace the object graph.
That walks all the live objects, which can touch a substantial
proportion of the memory. You also have to update marker bits; if you
use multiple marking threads, they can contend for the marker bits.

The worst case is when you have a very large object graph, with lots of
live data, and just a sliver of garbage. Now you must trace the entire
graph, but you get very little benefit.

There is a "generational hypothesis": objects allocated since the last
collection are more likely to become garbage than objects that have
survived a collection. That means that tracing the recently allocated
objects will give you better bang-for-your-buck for identifying garbage.

To implement generational GC, you must have a young generation space
separate from the old generation space. You allocate in the young
generation. When the young generation space is full, you trigger a
"minor" collection. This is typically a copying collector, which copies
as it traces. It uses forwarding pointers. It only traces *young*
objects, it does not follow references into the old space.

But the root set may contain references from the old space to the young
space. These will need to be updated by the minor collection copying
collector. How do you track these references? You need more write
barrier logic. When you store a reference in an old generation object,
you check if the reference is into the new generation. This is usually
as simple as a pointer comparison, since the new generation is a
contiguous block. If the reference points into the new generation, you
store this location in a *remembered set* of locations to check for
`old->new` references.

## Cards

You can optimize this. The write barrier, instead of appending to a
remembered set, can "mark" a card dirty. A card in HotSpot is 512bytes.
A dirtied card indicates that the GC must re-examine the block at trace
time for `old->new` references to add to the remembered set. After
scanning and adding reference locations to the remembered set, the GC
thread can mark the card clean again. Marking a card is faster for a
write barrier than appending to a remembered set. More GC work is moved
out of the mutator to the GC thread. And there is the potential for some
coalescing/absorption of writes to the same location.

To be clear: the generational hypothesis must also include that
`old->new` references are rare. Else we would end up scanning every
card...

## Eden, FromSpace, ToSpace

Where will we copy objects that survive a minor collection? We could
immediately promote them into the old generation.

More commonly, we keep the objects in the young generation a while. The
new generation space is divided into:

1. Eden: a free space for fast allocation,
2. FromSpace: a space in which young objects that have survived at least
   one minor collection still live,
3. ToSpace: a space into which we can copy objects that are selected to
   remain in the young generation.

When we do a minor collection, we track how many minor collections an
object has survived in the object header. HotSpot tracks this in the
object header. If this count is below a threshold, we copy the new
generation object into ToSpace. If it exceeds the threshold (or we run
out of ToSpace), we allocate space in the old generation and copy there.

# Regions and G1 collector

The region idea is somewhat like an extension of generational GC. In
generational GC, you have a new generation "region" in which objects
that have only survived a few collections live. You collect this region
more often than the old generation region, because you expect collection
of the new generation to be more profitable: more space reclaimed per
live object copied.

The region idea is a kind of extension to this. You split the heap space
into regions. HotSpot aims to have ~2,048 regions, of size between 1MB
and 32MB.

Some of the regions will be used for the Eden, FromSpace, and ToSpace.

The other regions hold old generation objects. For every region, you'll
keep remembered sets of references pointing into the region. You can use
the dirty card idea as an optimization.

There are two kinds of collections. A "minor" new generation collection
works as usual. The other kind of collection is called a "mixed
collection". This begins with a concurrent trace of all live objects
(new and old). You can use either the incremental-update or SATB
approaches (Java's G1 collector uses SATB). By the end of the trace, you
have an *estimate* on the number of live objects in each region, and the
amount of free space.

## Region Evacuation

When you're done tracing, the collector chooses to *evacuate* some
regions for re-use. It stops the world, and begins a copy-collection
from a chosen region into other regions. This compacts the live objects
from the region. You must update references to objects that lived in the
evacuated region. You use forwarding pointers as usual. You must iterate
the dirty cards/the remembered set to find pointers into the region from
outside it.

G1 estimates the amount of space that can be reclaimed (the "benefit").
It estimates the "cost" in terms of number of live objects to be copied,
and number of references to be updated. G1 uses these (and other
factors) to pick what it thinks are the most valuable regions to target
for evacuation.

The easiest way to evacuate some selected region is to *stop the world*
during the evacuation. That is what G1 does. The downside is that the
evacuation is not concurrent with the program. But G1 *does* make
collection *incremental*. Moreover, G1 has a knob which sets an intended
limit on how long it will stop the world to evacuate regions.

G1 can hope to meet its target duration because it can estimate how long
it may take to evacuate a region (as discussed above). And regions are
supposed to be small enough that if we choose just one region to
evacuate, hopefully we can evacuate it within the intended time limit.

G1 may do multiple evacuation pauses, each intended to stop the world
for no more than the finite target duration.

Eventually, there is logic to decide when it is no longer worthwhile to
keep collecting more regions, at which point region evacuation stops. At
that point, G1 uses some adaptive heuristics to estimate when it must
next trigger a concurrent trace in order to be able to be able to
complete the next GC cycle before memory runs out.

## Throughput Analysis

The primary goal of the regions is to make the live-object
compaction/contiguous free space reclamation incremental. That is
achieved, and lowers latency to the program/mutator.

You might also improve throughput (in terms of bytes recovered per CPU
cycle) if you can target regions where there is a lot of garbage
relative to the number of live objects. But unless something is odd, you
probably cannot consistently select a region with a higher proportion of
garbage than the long term garbage/live object ratio across all regions.
Even if in the beginning there is variance in the percentage of garbage
in each region, over time this distribution presumably becomes uniform.

Note that because you must either keep remembered sets or dirty cards,
there is additional overhead relative to a STW copy compaction of the
entire live object graph. That is what the Java "Serial" and "Parallel"
collectors do at the end of a major collection.

In summary, we expect G1 to reduce variable latency imposed on the
program by compaction. But we expect that G1 may have inferior
throughput relative to the "Serial" and "Parallel" collectors. To
summarize, the point of the regions is to make compaction incremental.
It is not really about unlocking increased throughput over
non-incremental fully-STW compaction.

# Garbage Collection In Various Languages

## Java

- Serial: stops the world. Generational. Runs copy collector on young
  generation, runs mark-compact on old generation.
- Parallel: mostly the same as serial, but runs many parallel GC
  threads.
  - Parallel is typically superior to serial, unless the heap is quite
    small or the number of cores quite low.
- CMS: Concurrent Mark-Sweep
  - Generational.
  - Does an initial STW to mark stack gray.
  - Then does concurrent tracing, using incremental-update style
    barriers.
  - Does a final STW to trace from new stack.
  - Last, it does a sweep. It only sweeps and does not compact, so this
    is easier to do concurrently. Dead objects are swept onto free
    lists.
  - CMS is a low-pause collector. But for long-lived apps, it's bad that
    memory can fragment more-and-more. Eventually, a large allocation
    may not have a free space large enough. Then Java must STW and do a
    compact of the old generation.
- G1 ("Garbage First")
  - Traces concurrently. Uses SATB write-barriers.
  - It is generational.
  - Splits the heap space up into regions.
  - Because it compacts when it evacuates regions, it creates larger
    contiguous blocks of free space. But it doesn't have to compact the
    *entire* live object tree.
  - Like CMS, this is low-pause. But G1 is better at producing large
    blocks of free space.

# TODO

- Useful: http://www.memorymanagement.org/glossary
- Very Comprehensive: "Uniprocessor Garbage Collection Techniques"
  (Paul Wilson)
  - https://www.cs.cmu.edu/~fp/courses/15411-f08/misc/wilson94-gc.pdf
  - https://users.cs.northwestern.edu/~pdinda/ics-s05/doc/dsa.pdf
- Replicating collector: copying without destruction. Useful in
  incremental/parallel collection??
- Buddy System?
- Baker's Treadmill

- Java Lowest latency collectors
  - ZGC (inspired by Cliff Click, Azul) and Shennandoah
  - Both do moving/compacting concurrently. They work differently, but
    have same goals.
  - Try to eliminate STW during evacuation. That further reduces
    tail latency.
  - But typically these pay for less latency imposed by GC by reducing
    throughput. Because it will have to use more atomics probably.
  - ZGC is only going to be attractive when latency target is less than
    20ms or so (the target GC pause time).
  - If you have extra CPU performance to spend, then you might prefer to
    spend more CPU no ZGC to reduce latency spikes from GC.
  - With 64 cores, a 20ms global G1 pause costs more throughput. But you
    can also sweep faster with more cores, because G1 is parallel. On
    net, the question isn't about throughput. OTOH, the global pause
    will create many parallel latency spikes.

- Project Valhalla from Java. It wants to have value types.
  - .NET has them. They don't need to be traced.
- Other languages:
  - Node/V8: Generational tracing GC, mark-compact
  - Ruby: mark-sweep
  - Haskell: generational copying GC.
  - Go: concurrent non-moving mark-sweep
  - BEAM/Erlang/Elixir: per-processor generational semi-space copying GC.
  - Swift: Automatic reference counting.
  - C++ shared_ptr in C++ is itself expensive because of atomics.
- Talk about p99 when it comes to low latency goals.
  - Also: how does G1 decide what's "fair" for mutator vs collector.
  - As in: it sets a ceiling for duration to stop world. But how long
    will it resume program before doing more STW evacuation?
- Discuss why need read barriers for concurrent relocation.
