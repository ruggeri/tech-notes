It's generally not decidable what objects will be used again in the
life of a program. Two main heuristics:

* Those things that aren't referenced by any objects.
* Those things which aren't reachable from a set of objects on the
  stack.

## Reference Counting

The main problems with reference counting are:

* Can hypothetically overload the reference count. Bloats the object.
* Every mutation of a pointer needs to decrement one reference count
  and increment another, increasing memory bandwidth usage, and
  hurting caching.
    * You can address caching problems this by doing program analysis
      to coallesce or defer count adjustments. Requires
      compiler/interpreter support.
* Unbounded pauses when you're destroying objects.
    * This can be addressed by incrementally decrementing reference
      counts. When an object's refcount falls to zero, put it in a
      queue, and another thread can then go on decrementing the
      items referenced by the object.
* Need locking or memory barriers in multi-threaded programs, hurting
  performance.
* Cycles are not collected.
    * This can be addressed if you very occasionally run a tracing
      garbage collector.

The advantages are:

* Simple.
* Can reclaim items as soon as they lose all references.
* Tracing GC performs poorly when you have very low free space,
  because you're triggering lots of collections that trace the entire
  working set. Also a trace kills the cache.

## Tracing: Mark-And-Sweep, Cheney Collection, Mark-Compact

Basic **mark-and-sweep** starts with a root set, then does a search,
marking each reachable item. Then the object list is iterated; all
unmarked objects are put on the free list (not reachable), while
marked objects have their bit reset.

The main disadvantage is that at present this algorithm is cannot be
run incrementally or in parallel. That means we have to stop
everything the program is doing when this runs. We'll fix that over
time. It also has the typical tracing problem of paging in all the
memory.

**Cheney Collection**

A two-space collector divides the address space into fromspace and
tospace. Memory is allocated in fromspace until full. Then, upon
collection, items are copied into the tospace. Upon completion, the
space tospace and fromspace roles are reversed.

Reducing fragmentation helps locality, and also allocations can be
satisfied more quickly. However, note that pointer arrithmetic will be
broken.

Cheney's algorithm is to:

1. Copy items referenced by the stack into the tospace.
2. Leave a "forwarding pointer", which references the new tospace.
3. Next, begin scanning through the tospace, looking at
   references. These will be into the fromspace.
4. Copy the referenced fromspace object if not done already. Leave a
   forwarding pointer.
5. Update the reference.

This is effectively a queue based BFS which handles cycles.

Apparently Cheney based his work on an earlier semispace collector,
but that must have been super unsophisticated, since Cheney's work
seems so obvious.

**Mark-Compact**

Cheney compaction will double the physical memory used, because of the
forwarding pointers. This sucks. Moreover, while Cheney compacts, it
might not put useful stuff next to each other, hurting locality. We
might prefer to keep things that were allocated adjacent still
adjacent after copying.

The Mark-Compact algorithm first runs a typical trace. It then moves
to the copying phase. *It does not use a second space*.

We'll iterate through live stuff in address order. We'll move each
item to the bottom of the range. To keep track of how we move stuff,
we'll write a pointer in a "break table", which is kept at the start
of unused memory.

However, as we copy stuff, we'll eventually need to copy into space
occupied by the break table. For that reason, we "roll the table": we
copy the first record to the end of the table, then move the object
over the old break table record location.

Rolling the table disorganizes it; we were adding records in
increasing memory address, so otherwise it would stay
ordered. Therefore, after the compaction is complete, we sort the
break table. We then iterate through the live objects, updating
references we encounter by doing a binary search in the table. This
means that the method is `O(n log n)` in the number of live blocks.

At first glance, we might say that the break table is as big as the
number of live objects, so we might worry that we have to save ~1/2
the memory for the break table. That was a problem with Cheney.

But this is a **break table**. We make a record for contiguous block
of live memory; not every object. A break in contiguous live memory
implies that there is space to store one more record in the break
table.

A couple takeaways: we don't need to reserve any extra space for the
break table, as in Cheney. Note that the break table size is at most
the number of live objects; this might happen if every live object is
followed by a dead one. In that case, one-half of the memory is used
by the break table; but the break table grows as we make space for it,
so we don't need to set aside space.

Note that sorting the break table is O(n log n). However, note that
this is linearithmic *in the number of breaks*. That's better than the
number of live objects.

## Tri-Color Marking

Let's get deeper into how you might trace incrementally. We haven't
gone into detail on tracing. Let's formalize three sets:

* White objects, which will be collected at end.
* Gray objects, which need to be scanned. This is empty at end.
* Black objects, which have been scanned and are retained.

You start out with the root set as gray. You scan gray items, moving
white items to gray. You blacken the item when you are done scanning
its outgoing references.

You can continue until everything is black or white; now you know the
white stuff can be thrown away. So far this is just the typical
tracing.

**Running Incrementally**

The idea is to be able to run this incrementally, without stopping
everything indefinitely. Basically, you let the GC trace for a while,
then give the program a chance to run. The biggest problem is that the
program might mutate objects and change references.

**Incremental update**

Every time the mutator mutates a reference from a black object, it
should re-gray the black object. This way you can never lose items
you'll still be needing. A cooperating compiler or interpreter will
have a **write barrier** that makes sure to gray black nodes on
reference assignment. Another possibility is to immediately gray any
object that gets written to a black object.

New objects can start out white.

Note that we never move from gray to white. Thus if a reference from a
black node is broken during the mark phase, we won't collect the
potential garbage. That's okay; we'll get it next time.

**Snapshot At The Beginning**

Alternatively, on a write to a gray or white node, we can immediately
gray the previously referenced object. This way we don't need to watch
writes to black objects. If a white object is assigned to a black
node, it must be currently referenced by some gray or white node. If
it never loses that reference, great: we'll trace and gray it
eventually. If it does lose that reference, we'll immediately gray it
in that case.

This is called "snapshot at the beginning" because anything reachable
at the beginning of the collection will be retained, even though it
may not be reachable by the end! Note that incremental-update can
collect some (but not necessarily all) referenced objects from the
beginning that become garbage during the course of the mark phase.

We must take care with new objects; they could be assigned to black
objects. Therefore, new objects should be colored black.

**The End**

In the SATB approach, if we are not tracing/collecting new objects,
then we will eventually we will exhaust the gray set, because only
objects predating the mark phase will ever be grayed.

In the incremental-update version, the gray set may keep growing as
more new objects are created and assigned to blackened
objects. However, if we use an allocation clock, where we do `T`
amount of tracing for `A` amount of allocation, we will exhaust
eventually if `T/A > 1`. Or something like that.

**Pros and Cons**

Probably lots. But one idea is that newly created objects are likely
short-lived, so it is probably preferable to assume they're white
because then it is possible to collect them promptly. Of course, if
the new stuff lives too long and gets traced, it'll be kept even if it
dies before the phase ends.

## Todo

* Useful: http://www.memorymanagement.org/glossary
* Very Comprehensive: "Uniprocessor Garbage Collection Techniques"
  (Paul Wilson)
* http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.47.275
* http://www.amazon.com/dp/1420082795
* http://www.amazon.com/dp/0471941484

* Generational Garbage Collection
    * Inter-generational pointers??
* Parallel Collection (those are two different things!)
* Replicating collector: copying without destruction. Useful in
  incremental/parallel collection??
* Free Lists, Buddy System?
* Baker's Treadmill
