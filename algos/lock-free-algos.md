# Concurrent Data Structures

Concurrent data structures allow for multiple "concurrent"
users/mutators. Even before CPUs started to gain parallel execution, you
might have multiple OS _preemptible_ threads running on a single
execution unit ("core").

Operations must be careful about concurrent modification of the data
structure to ensure _(thread) safety_. A thread could be interrupted in
the middle of an operation. If you start mutating an object in one
thread, you might not be able to use the object yet in another thread
because the invariants may not have been restored yet.

One way to provide thread safety is through _locking_, also called
_mutual exclusion_. Coarse-grain locking can ensure safety, but it may
prohibit parallel use of the datastructure. Finer grained locking might
improve breadth of parallel use, but introduce the possibility of
_deadlock_ (a form of "liveness" failure).

We say that execution is _parallel_ if two threads can be running and
performing operations simultaneously (at least two separate execution
units). Here is a key difference between a truly parallel and a
"single-core" concurrent environment: the single-core threads can be
imagined to have a "atomic", "global" model, where a write from a thread
will be immediately visible to a read by any thread that is run
afterward. True parallel execution means that reads/writes may give
values that are not consistent with any single order of atomic
operations on a global memory.

An alternative to locking/mutual exclusion is _non-locking_ (AKA _lock
free_) techniques. In lock free code, a thread cannot be blocked from
progress by a suspended thread that possesses a required lock.

Note that locks/mutual exclusion is typically implemented _in terms of_
non-locking primitives; locks are not themselves primitive. So we will
eventually try to be quite precise about what it means to be truly lock
free. Else we could introduce/implement locking via "lock free"
primitives without noticing it.

# Disadvantages of Locks/Mutual Exclusion

- Contention: when someone has the lock, other people can't do anything
  when that thread goes to sleep. That's kind of annoying, because the
  OS isn't necessarily going to know that threads can't make progress
  before switching.
  - This is especially problematic if the thread dies without releasing
    the lock, or if it enters an infinite loop.
  - OTOH: those are actual bugs. But they could be real if client code
    is executed while you have a lock.
- Overhead: locks tend to be expensive to obtain relative to lock free
  test-and-set stuff. When contention is low, locking can be
  unnecessarily expensive.
  - On the other hand, when contention is high, lock free ideas like
    optimistic attempt and possible retry (e.g., ideas based on
    test-and-set) can be wasteful.
- Bugs: Difficult to prevent deadlock. Difficult to debug. Locks can
  "compose" poorly; you may inadvertently cause deadlocks.
  - On the other hand, lock-free code can be difficult to ensure doesn't
    have subtle safety problems.
  - Even when lock-free code works as designed, you still may not be
    able to naively compose operations into "isolated" transactions.
- Priority inversion: if a low-priority thread has a lock, it may stop
  high-priority threads from running.

Alternatives include:

- Message passing for synchronization.
- Software transactional memory.
- Other custom lock-free, wait-free data-structures.

# Lock Free

Lock-free is defined to mean that when you suspend some threads, then at
least one active thread must be able to make progress. Not everyone can
be stopped just because someone with a lock is paused. In particular: if
you suspend _all but one_ thread, then that thread must be able to make
progress and complete. Technically, this is a weaker property called
_obstruction free_. It is stronger to say: if any subset of threads are
allowed to run (and some other subset are slept), always one must
eventually make progress. That is true lock free, and it implies
obstruction free.

Lock-free code can't have locks; else a suspended holder of a lock would
block operations that need that lock.

Lock free ideas often involve making copies of structures, updating your
thread-local copies, and then doing a compare-and-set atomic operation
to "replace" old versions with updated versions. This is called
_optimistic_. If there were a concurrent change and the compare-and-set
failed, you _retry_.

Lock-free code cannot suffer deadlock as locking code might. That is a
form of safety. Lock-free _may_ also have higher _throughput_. Readers
can often proceed faster because they don't need to claim read locks to
block writers. Implementation of locks inevitably involves some form of
atomic operation, which is not cost-free. If lock-free code can avoid
_any_ atomic operation on the read path, that can help read throughput.

With lock-free code, writers might proceed faster because they are not
blocked by readers. Even if you were to use a _readers-writer_ (RW)
lock, you would not get this advantage of unblocked writes.

But lock-free code doesn't guarantee better _throughput_. While that is
often the case under certain workloads, it is not the _definition_ of
lock free. Lock free might have more indirection, or be more
complicated. It can be mentally trickier than code that can rely on
mutual exclusion. And when write contention is high, threads might be
frequently interrupting each other, rolling back each other's work. Even
if no one gets starved, throughput can be trashed.

A thread might even be repeatedly interrupted, over and over, thus
prohibiting it from making progress for an arbitrarily long time. Even
though the system as a whole may be making progress, an individual
thread might be _arbitrarily_ delayed. If a thread can be arbitrarily
delayed, this is called _starvation_. Lock free code may still have
starvation.

A form of "mutual starvation" might occur if two threads keep trying to
make progress, but keep causing each other to retry, without either ever
proceeding to completion. In this case, we call it _livelock_. True lock
freedom forbids live-lock, but obstruction free code does not. Indeed,
livelock is most commonly associated with locking code where you try to
take multiple locks, but release them and retry instead of waiting when
you realize someone has a lock you need.

Let's think about how lock-free might be implemented. CAS is the typical
approach. Say you have a persistent data structure under the hood. You
make a new copy with your changes locally, then you do a CAS to mutate
the shared pointer to refer to your version. If the CAS fails, you start
again.

It is possible to be arbitrarily delayed. But throughput overall is
guaranteed, since for your work to be wasted, someone else's had to be
completed.

A good example: a map with many readers but few writers. You don't
want to slow down read access with a lock (even a read-only lock). So
you let a writer make a copy and CAS. This imposes minimum delay on
readers. Basically I'm describing STM or MVCC here! NB: this approach
works best when you have (1) persistent structures and (2) garbage
collection to prevent races involving deleted data. (I think Treiber
stack below illustrates this).

# Liveness Summary

- Obstruction free: a thread that runs alone "long enough" must
  eventually complete.
- Deadlock free: system cannot get in a state where no one can make
  progress.
  - Livelock is a form of deadlock.
  - Deadlock free is stronger than obstruction freedom, since we don't
    need to schedule a thread without interruption for "long enough".
- Lock free: even if you sleep some threads, amongst the other
  scheduleable threads, one will complete an operation.
  - Basically, no one thread can hold a lock that prohibits all the
    others from making progress until the "lock holder" finishes.
  - Sometimes we say "infinitely often a thread will complete an
    operation." That just means: threads will keep making progress, not
    just once, but over-and-over forever.
  - Lock free is stronger than deadlock free, because progress is
    guaranteed even when some subsets of threads are slept indefinitely.
- Starvation freedom: even if you sleep some threads, amongst the other
  scheduleable threads, _each_ will eventually complete an operation.
  - No one is going to get stuck waiting _forever_.
- Wait freedom: even if you sleep some threads, amongst the other
  scheduleable threads, each will eventually complete an operation
  _within some upper bound number of steps_.
  - Basically: you cannot be kept _arbitrarily waiting_.
  - Other threads may make progress before you, but "later" work cannot
    indefinitely jump over you.

# Wait Free

We've seen lock-free code can suffer from arbitrarily delays to a
single thread. A stronger guarantee is _wait-free_. This guarantees
that every operation will succeed in a fixed number of steps: no
operation can be arbitrarily delayed. This is possibly useful for
"realtime" tasks; normally there is additional overhead for wait-free
algorithms.

There are "universal constructions" that translate serial code to wait
free code. This shows that anything can be made wait free. However the
overhead of these universal constructions, though demonstrating the
possibility, is not actually practical.

Apparently it is fairly difficulty to make practical wait-free
structures; a recent paper presented (what they claim is) the first
practical wait-free queue.

# Treiber Stack

A lock free stack uses compare-and-set. Basically you create a new node
and do a compare-and-set to the front ptr. Likewise for pop.

This is lock-free, of course. Note that it is neither starvation nor
wait-free, as the threads could keep successfully CASing the head, while
the starved thread keeps failing over and over.

Note that the Treiber stack, while lock free, does not at all allow for
parallel modification. So multiple writers will necessarily serialize.
In fact, with heavy write contention, it might be better for throughput
just to lock.

**ABA Problem**

One thought: you have to be a little careful of the "ABA" problem.
Here's an example: thread1 wants to pop the top of the stack (node A),
it wants to set the top to node B. Before it does a CAS to make sure A
is still the top, a 2nd thread interrupts. It pops A, pops B, resets
`A->next` to `B->next`, and then pushes A back on. The 1st thread now
resumes. The `CAS` will still see that `stack->head` is `A`. But if it
isn't very careful, it will set `stack->head` to `B` rather than
`B->next`.

In part, the problem is due to concurrent modification of an object
still accessible from another thread. Maybe we could mitigate the
problem if the API only allowed push/pop of _values_.

But ultimately, memory can _always_ be reused by the runtime. An address
isn't tied to a specific object created at a specific time; it's just a
place in memory, which can be reused when other objects are placed there.

Herlihy and Shavit suggest that you might keep an incrementing "version"
number in the node, to try to be safe for re-use of a node. Java does
give a CAS primitive which lets you keep a "marker" for the version of
the object. They note that the version number can wrap entirely around,
causing problems, but Wikipedia suggests that if enough bits of tag are
used, wrap-around cannot happen for many years at current CPU speeds.

**Deletion/Garbage Collection**

I believe this comes from Aaron Turon's notes ("Lock-freedom without
garbage collection")...

Another problem: what about deletion (that is, garbage collection) of
popped nodes? Specifically, let's focus on the _node_, not the value. I
want to focus on the node so that we don't confuse ourselves with
ownership semantics of values.

We could try to delete the node after the CAS in the pop. But then
other threads trying to do a pop might try to access `node->next` when
they do their own CAS (which is supposed to fail, signalling they
should retry a pop). But node was deleted: use after free!

We can solve this with GC of course. Another possibility: whenever
popping, list this ptr as dead in an epoch list. Each thread is in an
epoch. Every time a thread attempts a pop, it updates itself to the
current epoch. Every once in a while, a thread will check to see if
everyone is in the current epoch. If so, it does a CAS to increment
the epoch (so that it knows it's the only person updating the
epoch). If that is successful, it can delete data from 2 epochs prior,
since it knows everyone is past that old epoch. It can't delete data
from the previous epoch, since not everyone is out of it yet...

As of 2026-04-20, I am not sure if I remember what these old notes about
epochs are supposed to mean. They seem to be a reference to "Practical
lock-freedom", a thesis by Keir Fraser.

Source: https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-579.pdf

# Queue with Two Locks

It's simple to make a queue concurrent by adding a lock for any
enqueue/dequeue operation. Say we want to be able to dequeue while we
are enqueueing items. Then we want a lock for both the head and the
tail.

The idea is this. We'll store items in a linked list. Start with an
_empty_ node in the queue; we'll always have at least one node in the
queue, an "empty" node at the head of the list.

When enqueueing, take the "enqueue" lock, create a new node, write the
value in. Update `tail->next` and `tail`. When dequeuing, take the
"dequeue" lock, remove the head node (update `head`), _but return the
next value_ (`head->next->val`). If you want, you can erase the value in
the next, thus restoring the invariant. This trick is from Michael and
Scott.

By using this trick, you always have at least one node in the linked
list (the dummy). With this trick, `dequeue` never modifies the `last`
field (in particular, doesn't modify `last` to `NULL` when last item
taken) and `enqueue` never modifies the `head` field (in particular,
doesn't modify `head` to a newly created node when a value is enqueued
in an otherwise empty list).

When dequeueing, a thread can see that the queue is empty if
`head->next` is `NULL`. Dequeue can indicate failure to dequeue through
exception or boolean or whatever.

**Can You Avoid Dummy Node?**

I believe that you can avoid the empty node trick possibly:

- When there is just a single item, `dequeue` can add tail lock and set
  `head = tail = NULL`.
- When there are no items, `enqueue` can drop tail lock, and try to grab
  head then tail locks to set `head = tail = new Node(val)`.
  - Important to abort and retry, rather than just add `head` lock, else
    deadlock might occur. Locks must always be acquired in the same
    order!
- This doesn't introduce deadlock, but...
  - It definitely adds some complexity. Possibly more branching and thus
    slower CPU performance.
  - When queue has approximately one item, effectively serializes the
    queue. Taking an item will block adding an item...
  - If a thread blocks on `dequeue` of an empty queue, `enqueue` cannot
    complete...
- In sum: there probably is no advantage to eliminating the dummy node.

**Cache Coherency**

- If you write in one thread, it may not be seen in another, because of
  cache incoherency.
  - It really depends on the cache consistency model of the
    architecture.
- Producers will see each others' work enqueueing because they
  synchronize via mutex.
  - Lock/unlock of the _same_ mutex will create a happens-before
    relationship between threads.
  - All writes from a previous mutex holder should be visible to reads
    of a later mutex holder.
- Likewise, consumers will see each others' work dequeueing.
- But how do we know that a consumer can see the items that a producer
  has pushed on?
  - There _does_ need to be some coordination between
    producers/consumers. And this isn't happening via a mutex, since
    producers and consumers have separate mutexes.
  - I believe the idea is that producers will do `atomic::store` on
    `tail->next`.
  - When consumer looks for next node, it will do an `atomic::load` on
    `head->next`.
- The cost of the atomic operations really depends on the
  cache-coherency model of the architecture.
  - On x86, a write from one unit will always (eventually) invalidate
    cache lines held by other units.
  - So the atomic store/load operations are just regular ops.
  - So the only thing these atomic ops might do is ensure that C++ does
    not _reorder_ operations.
    - For instance, if a producer creates a node, sets `value`, and then
      modifies `tail->next`, we must make sure that `tail->next` is not
      set until after `node->value` is set. C++ _might_ reorder this
      unless we tell it something about memory ordering.
    - x86 is nice enough that if we can see a later store, we are
      guaranteed to see earlier stores. So it is compiler reordering we
      particularly need to protect against.
- In sum: when implementing concurrent algorithms/datastructures, the
  papers may assume a stronger cache coherency model and memory ordering
  model than the hardware supports.
  - But it seems that x86 is actually quite strong.

**Condition Variables**

To have a blocking queue, you could have the `dequeue` thread spin until
a value is available. But can you sleep the thread until an item is put
on the queue?

You can try to use a `condition_variable`, and have the producer wake
the consumer when something is put on. However,
`condition_variable#wait` requires you pass a mutex (the `head` mutex is
a natural choice). But it's also _required_ that a change to the
underlying condition happen _while holding_ the same mutex (before you
call `notify_one`). That seems to imply that the producer should _also_
hold the `head` mutex, which mostly defeats the point of our queue.

This is to prevent a "lost" wakeup. To wait: you must lock, check
condition, register thread, unlock, sleep. To notify: you must lock,
change condition, unlock, wake.

Maybe a better choice would be `counting_semaphore`; it has a `release`
method that increments a count, and an `acquire` method that decrements
it. You could imagine this be implemented from a condition variable and
a `item_count` variable.

Using a `counting_semaphore` will definitely make this a hot variable.
That will hurt performance because of cache invalidation/misses. The
amount of hurt in part depends on how much time is spent doing the
enqueue/dequeue work. If enqueue/dequeue is about as fast/slow as
acquire/release, then managing this hot variable may be really tanking
throughput. You might start to consider using just one lock.

But, if you stop using two locks, then a suspended producer will block
consumers. So there is still value in adding concurrency, even if
throughput might be reduced.

- Source: ChatGPT 2026-04-20.

# Lock Free Queue: Single thread on each side

Consider a linked list, with a dummy value as before. As we saw, the
consumer never interferes with a producer. The locks actually just
coordinate people on the same side. So the locks aren't needed if you
have a single producer/consumer on each side.

You can do this in an array if your queue is of bounded size and the
producer should just block when full. In that case, the `next` is always
the next idx in the array (with wraparound). You can instead set a bit
per idx for whether there _is_ a successor item. In fact, you only need
a single int to specify the idx of the last item.

# Lock Free Queue: Multiple threads on each side

This is also in the Michael Scott PODC paper.

Same idea of keeping a dummy node. To enqueue, first check that
`queue.tail` really has `queue.tail->next == NULL`. If not, the tail is
"lagging. Keep using CAS to advance the `queue.tail` forward.

When `queue.tail` really is advanced to the end, do a CAS to
`queue.tail->next` add a link. That can only fail if someone made a
concurrent append. In that case, just try again.

When the append finally succeeds, you try to update `queue.tail` with
CAS. This is just to be nice; if the CAS fails, you still just exit.
Frankly, I don't think it is necessary for correctness (since later
enqueue operations will advance a lagged tail).

Likewise, on the head side, you check to see if the list is empty. This
might occur if `queue.head == queue.tail`. But that can also happen if
tail is lagged, so `dequeue` operation must also try to CAS to advance
lagged tail. But if queue is not empty, you do a CAS to set the head to
the next. If successful, free the head node. Else someone did a
concurrent dequeue and you must try again.

Enqueue operations must advance a lagging tail for lock-freedom else a
suspended enqueuer that hasn't yet advanced the tail will block other
enqueuers. Likewise, dequeue operation must be able to dequeue, which
means more than just checking that `queue.head != queue.tail`, because
they can be equal (lagged tail) but there are items to dequeue.

## Lock Free Singly Linked List

This is due to Harris. Want to have arbitrary insertion/deletion at any
point. Insertion between `n1` and `n2` is easy. You create `x =
Node{val=val, next=n2}`. You then CAS on `n1->next`.

Supporting deletion is a little tricky. To delete `x`, you could try to
do a CAS on `prev->next` to `x->next` (thus protecting against
possibility that that prior node had someone insert afterward). But what
if, concurrently, someone has modified `x->next` by either inserting
after you, or deleting the next item? In that case you would lose the
insert, or restore the deleted item.

One solution is to do a CAS to _mark_ `x->next`. You do this by popping
a high-order bit reserved for marking. This steals a bit from the
pointer, and must be masked out for dereferencing. Insert after logic
should check the marker bit; if it is marked, you know this node is
being deleted. You must retry from the beginning.

After marking `x->next`, you may CAS on `prev->next`. If `x->next` has
been updated to a new node (because of concurrent insertion after `prev`
and before `x`), you can scan forward. However, if `x->next` has itself
been marked, you give up on cleaning out this node. It's okay to leave
it in the list. Future scans through the list should try to clean out
nodes they encounter that are marked deleted.

Harris uses this to implement a set; he keeps the keys in sorted order
and just scrolls through again to the right position on every fail.

## Hash Map

In order to do a "non-extensible" hash map, you just need a set of
buckets, each of which implement a set.

Harris showed how to do a set as a sorted linked list. It's kind of
annoying that `insert-after` can require a re-scan of the array on a
failed CAS, but that's not a huge problem. These lists are supposed to
be very short anyway.

I spent a lot of time working on a simpler list where you can only
insert at the head. In that case, you don't have to worry about lost
inserts as in Harris. But I don't see the practical benefit.

Maged basically did what I did, resulting in a "non-extensible"
concurrent hash map.

Lea wrote the ConcurrentHashMap for Java, but this is not entirely
lock free. Basically, writers typically don't block readers, there are
32-write locks (for 1/32nd of the buckets, I think), and only need to
lock the whole thing on resize. Java's HashTable, by contrast, has one
lock, and it blocks readers, too. (You can increase number of locks to
support more concurrency; why isn't that sufficient?).

Cliff Click has basically a closed-addressed array. He talks about how
to do a concurrent resize. Basically, when you start the resize, you
start telling people to look first in the old version, and maybe also
in the 2nd version. As your threads do some copying to the new
version, they mark the old version as "moved". Basically, you're going
to every address in the table and marking it as "moved".

Shalev-Shavit have an idea of a "split-ordered list". Basically, you
assume that hashes are taken modulo `2**i`. Then you realize, on each
resize-doubling, each element in a bucket will stay in bucket `k` or
move to `2**i+k`. So each element is each time sent left or right. The
ordering is to keep left stuff before right stuff. Then it's just a
matter of maintaining an array of "hints" into the concurrent
list.

The problem is that the number of hints needs to grow as you add
buckets. And it needs to be in an array so you can get O(1)
access. But how do you grow that array?

You can use a high branching tree. The first node represents buckets
`0...2**i`, to create a next node, create a second array for buckets
`2**i...2**(i+1)`. You also need to create a parent node, where the
first two references are to these tables.

Basically, this is sort of like a persistent vector. Notice that there
is a little overhead in jumping through tables, but this can be very
low.

NB: you have to update a count on every insert. That is a point of
possible contention. But note that the greatest majority of insert
time is spent hashing; so contention on a simple increment shouldn't
be very high, unless we have _many_ processors.

Another alternative is a ctrie; which is basically a HAMT updated in a
CAS manner. The one trick is to handle concurrent modifications to the
same node without losing other updates. To do this, they add
intermediary nodes between every pair of nodes.

- Ctrie: http://infoscience.epfl.ch/record/166908/files/ctries-techreport.pdf
  - From 2011; maybe more
- Ctrie: http://lampwww.epfl.ch/~prokopec/ctries-snapshot.pdf
- Split-ordered list: http://cs.ucf.edu/~dcm/Teaching/COT4810-Spring2011/Literature/SplitOrderedLists.pdf
  - From 2006

I think that these hash maps should have good opportunity for
scalability.

## More Notes

- On modern machines, when writing a single word, another thread can't
  see a partial update. Read/Writes of a single word are atomic.
- TODO: Memory ordering can be a problem.
- Futex is "fast userspace mutex". It uses some shared memory and
  atomic operations so that taking the uncontended lock happens in
  userspace. If that doesn't work, then the thread needs to call into
  the kernel to puts itself on a kernel waitqueue.

## References

- http://www.cs.tau.ac.il/~shanir/concurrent-data-structures.pdf
  - Excellent summary. Covers stacks, queues, linked lists, hash
    tables, search trees, priority queues.
  - But it is kind of a literature "review". It points you to the
    original papers, not describing the datastructures themselves.
  - Link died. I think it is this chapter from Mark Moir and Nir Shavit.
  - https://people.csail.mit.edu/shanir/publications/concurrent-data-structures.pdf
  - Chapter is From: http://www.amazon.com/Handbook-Structures-Applications-Computer-Information/dp/1584884355
- Alex Andrescui (Basic ideas)
  - http://www.drdobbs.com/lock-free-data-structures/184401865
  - Link died. Here is a snapshot: https://archive.ph/ylq4p
- Herb Sutter (I actually didn't find these too useful)
  - http://www.drdobbs.com/cpp/the-trouble-with-locks/184401930
  - http://www.drdobbs.com/cpp/lock-free-code-a-false-sense-of-security/210600279
  - http://www.drdobbs.com/parallel/writing-lock-free-code-a-corrected-queue/210604448
  - http://www.drdobbs.com/parallel/writing-a-generalized-concurrent-queue/211601363
- Buildling a locked queue
  - http://www.ibm.com/developerworks/aix/library/au-multithreaded_structures1/index.html
  - Link died: https://archive.ph/http://www.ibm.com/developerworks/aix/library/au-multithreaded_structures1/index.html
  - People note that the two-lock queue is incorrect! It doesn't
    have the Michael/Scott trick.
- Building a (half-assed) Treiber stack
  - http://www.ibm.com/developerworks/aix/library/au-multithreaded_structures2/index.html
  - I have a lot more detail than this...
- Michael and Scott paper
  - Covers two-lock and CAS based queues.
  - https://www.research.ibm.com/people/m/michael/podc-1996.pdf
  - Link died. I think it is this: https://www.cs.rochester.edu/u/scott/papers/1996_PODC_queues.pdf
- https://aturon.github.io/blog/2015/08/27/epoch/
  - Had good info about epochs.
- https://wiki.eecs.yorku.ca/course_archive/2007-08/F/6490A/_media/presentations:a2pres.ppt
  - Two lock queue powerpoint with images!
- https://www.youtube.com/watch?v=HJ-719EGIts&feature=youtu.be
  - Cliff Click describes Azul's version of a concurrent HM. It's
    lock free and does resizing.
  - A lot about memory fencing.
  - I have an email thread where Doug Lea is considering updating
    the CHM to either Click's or Shalev-Shavit. So those do seem
    like two leading contenders.

## More Books

- bought these.
- http://www.amazon.com/Art-Multiprocessor-Programming-Revised-Reprint/dp/0123973376/ref=sr_1_1?ie=UTF8&qid=1454457397&sr=8-1&keywords=The+Art+of+Multiprocessor+Programming
- http://www.amazon.com/C-Concurrency-Action-Practical-Multithreading/dp/1933988770/ref=pd_sim_14_2?ie=UTF8&dpID=51nuLYxU2iL&dpSrc=sims&preST=_AC_UL160_SR128%2C160_&refRID=0YEA0ZVEF4BAN3S843WM
