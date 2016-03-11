## Concurrent Data Structures

Historically, these were datastructures which would allow concurrent
calling of various methods. However, did not necessarily support
parallelism. This mattered for pre-emptive multitasking on a single
CPU for time-slicing.

With concurrent data structures, you have to be careful to ensure
safety. If you start mutating an object in one thread, you can't use
that in another thread because the invariants may not have been
restored yet. To ensure a higher level of safety, you run into the
risk of sacrificing liveness: you don't want your data structure to
get wedged.

Nowadays, with parallel execution, the term has expanded to include
what used to be separately denoted *shared data structures*. These are
ones which are also safe to use in *parallel* code, where multiple
threads might be running.

To acheive this, you might use locks, which may limit
throughput. Alternatively, you might use non-locking (AKA lock free)
algorithms.

## Disadvantages of Locks

* Contention: when someone has the lock, other people can't do
  anything when that thread goes to sleep. That's kind of annoying,
  because the OS isn't necessarily going to know that threads can't
  make progress before switching.
    * This is especially problematic if the thread dies without
      releasing the lock, or if it enters an infinite loop.
    * OTOH: those are actual bugs. But they could be real if client
      code is executed while you have a lock.
* Overhead: tends to be expensive to obtain locks relative to lock
  free test-and-set stuff. When contention is low, locking can be
  unnecessarily expensive.
    * When contention is high, test-and-set can be wasteful.
* Bugs: Difficult to prevent deadlock. Difficult to debug. Locks can
  "compose" poorly; you may inadvertanly cause deadlocks.
* Priority inversion: if a low-priority thread has a lock, it may stop
  high-priority threds from running.

Alternatives include:

* Message passing for synchronization.
* Software transactional memory.
* Other custom lock-free, wait-free data-structures.

## Lock Free

Lock-free is defined to mean that when you suspend threads, then some
active thread must be able to make progress. Not everyone can be
stopped just because someone with a lock is paused; thus lock-free
code can't have locks! Locks cause all the problems above, so
lock-free may be desirable.

Intuitively, lock free might works because when a thread is paused in
the middle of its work, other threads may "rollback" the other
thread's unfinished work. That gives other threads the opportunity to
make progress.

Lock-free code won't suffer deadlock as lock-free code
might. Composability problems or bugs in client code when a lock is
held are less severe. Contention imposes less cost.

Not everything is great. When contention is high, threads might be
frequently interrupting each other, rolling back each other's work. A
thread might be repeatedly interrupted, over and over, thus
prohibiting it from making progress for an arbitrarily long time. Even
though the system as a whole should make progress, an individual
thread may be very delayed.

Let's think about how lock-free might be implemented. CAS is the
typical approach (kinda like STM). Say you have a persistent data
structure under the hood. You make a new copy with your changes
locally, then you do a CAS to mutate the shared pointer to refer to
your version. If the CAS fails, you start again.

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

## Wait Free

We've seen lock-free code can suffer from arbitrarily delays to a
single thread. A stronger guarantee is *wait-free*. This guarantees
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

## Treiber Stack

A lock free stack uses compare-and-set. Basically you create a new
node and do a compare-and-set to the front ptr. Likewise for pop.

One thought: you have to be a little careful of the "ABA"
problem. Here's an example: thread1 wants to pop the top of the stack
(node A), it wants to set the top to node B. Before it does a CAS to
make sure A is still the top, a 2nd thread interrupts. It pops A, pops
B, and then pushes A back on. The 1st thread resumes, obviously
confused.

Basically, the equality check of the top can't ensure the stack wasn't
modified. The problem here arrises because you can push and pop
*nodes*. If you created a new node for every push, you'd be
okay. Except when your memory allocator re-uses that memory... Thus
you might keep an incrementing "version" number in the node, thus
you're safe for re-use of a node. Note that the version number can
wrap entirely around, causing problems, but that is extremely
unlikely.

This is called Treiber's stack. It's lock-free, of course. Note that
it is not wait-free, as the threads could keep successfully CASing the
head, while the starved thread keeps failing over and over.

Another problem: what about deletion of popped nodes? Specifically,
let's focus on the *node*, not the value. I want to focus on the node
so that we don't confuse ourselves with ownership semantics of values.

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

TODO: Treiber stack offers no opportunity for parallel
modification. It's good for concurrency, but it's not like two threads
can simultaneously be modifying.

## Queue with 2 Locks

It's simple to make a queue concurrent by adding a lock for any
queue/dequeue operation. Say we want to be able to dequeue while we
are enqueing items. Then we want a lock for both the head and the
tail.

The idea is this. Start with an empty node. When pushing, always add
items after the node. This operation just modifies the tail. When
shifting, remove the head node, *but return the next value*. If you
want, you can delete the value in the next, thus restoring the
invariant. This trick is from Michael and Scott.

The reason the trick is needed is because otherwise, when shifting off
the last item, you'd have to modify *both* the head and
tail. Likewise, when adding to an empty queue, you'd have to modify
both the head and the tail. This way, you only need one lock for
each. TODO3: Would it be okay if you *did* need to acquire both locks?

When someone tries to shift from an empty queue, you could raise an
error, or (more likely) otherwise indicate failure. To have a blocking
queue, you could have the shifter spin. But best is probably to use a
condition variable to wake the consumer when the producer puts
something on.

**Why do you need dummy?** Without the dummy, some operations require
you modify both head and tail, meaning you need to take lock on both
sides. I don't think deadlock can arise, since it shouldn't ever be
the case that *both* sides want to take both locks. But you are
basically serializing access to the queue when the queue has ~1 item
in it?

One possibility: if a thread is paused while adding an element to an
empty list, then we switch to another thread trying to do a
non-blocking shift, it will get blocked unnecessarily.

## Lock Free Queue: Single thread on each side

Consider a linked list, with a dummy value as before. As we saw, the
consumer never interferes with a producer. The locks actually just
coordinate people on the same side. So the locks aren't needed if you
have a single producer/consumer on each side.

You can do this in an array if your queue is of bounded size and the
producer should just block when full. In that case, the `next` is
always the next idx in the array (with wraparound). You can instead
set a bit per idx for whether there *is* a successor item. In fact,
you only need a single int to specify the idx of the last item.

## Lock Free Queue: Multiple threads on each side

Same idea of keeping a dummy node. To enqueue, you do a CAS to add a
link. If that fails, start again. If it succeeds, you need to update
the tail; it is currently "lagging". Do a CAS to update it, if that
fails, whatever.

This suggests a wrinkle. Before trying to do a CAS to add the a link
to the end, check to see if the tail actually points to a node with
NULL after it. If not, then do a CAS to move it forward. This helps
correct the lag.

It's not entirely necessary for correctness that you should try to
help by moving the tail forward. But it does prevent blocking: other
threads should not rely on the pusher to complete its work of updating
the tail. Otherwise, they couldn't make progress if that thread died.

Likewise, on the head side, you check to see if the list is empty. If
so, then you do a CAS to set the head to the next. If successful, free
the head node. I do think you have to be careful about GC as with
Treiber stack.

## Lock Free Singly Linked List

This is due to Harris. Want to have arbitrary insertion/deletion at
any point. Insertion is easy, just a CAS on the previous element's
next.

Deletion is a little tricky. You could try to do a CAS on `prev->next`
to your `next` (thus protecting against possibility that that node had
someone insert afterward). But what if, concurrently, someone has
modified your own `next` by either inserting after you, or deleting
the next item? In that case you would lose the insert, or restore the
deleted item.

One solution is to do a CAS to *mark* your next pointer, which signals
people not to add after you. Attempted `insert-after`s after a marked
node should fail. Concurrent deletions after a marked node don't need
to fail; though you should be careful when setting `prev->next` to
maintain the marking. Your delete may be "lost", in that the link may
not be removed, but at least it is properly marked and can be cleaned
up later.

To mark a pointer, we can just set a low order bit.

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
be very high, unless we have *many* processors.

Another alternative is a ctrie; which is basically a HAMT updated in a
CAS manner. The one trick is to handle concurrent modifications to the
same node without losing other updates. To do this, they add
intermediary nodes between every pair of nodes.

* Ctrie: http://infoscience.epfl.ch/record/166908/files/ctries-techreport.pdf
    * From 2011; maybe more 
* Ctrie: http://lampwww.epfl.ch/~prokopec/ctries-snapshot.pdf
* Split-ordered list: http://cs.ucf.edu/~dcm/Teaching/COT4810-Spring2011/Literature/SplitOrderedLists.pdf
    * From 2006

I think that these hash maps should have good opportunity for
scalability.

## More Notes

* On modern machines, when writing a single word, another thread can't
  see a partial update. Read/Writes of a single word are atomic.
* TODO: Memory ordering can be a problem.
* Futex is "fast userspace mutex". It uses some shared memory and
  atomic operations so that taking the uncontended lock happens in
  userspace. If that doesn't work, then the thread needs to call into
  the kernel to puts itself on a kernel waitqueue.

## References

* http://www.cs.tau.ac.il/~shanir/concurrent-data-structures.pdf
    * Excellent summary.
    * Chapter is From: http://www.amazon.com/Handbook-Structures-Applications-Computer-Information/dp/1584884355
* Alex Andrescui (Basic ideas)
    * http://www.drdobbs.com/lock-free-data-structures/184401865
* Herb Sutter (I actually didn't find these too useful)
    * http://www.drdobbs.com/cpp/the-trouble-with-locks/184401930
    * http://www.drdobbs.com/cpp/lock-free-code-a-false-sense-of-security/210600279
    * http://www.drdobbs.com/parallel/writing-lock-free-code-a-corrected-queue/210604448
    * http://www.drdobbs.com/parallel/writing-a-generalized-concurrent-queue/211601363
* Buildling a locked queue
    * http://www.ibm.com/developerworks/aix/library/au-multithreaded_structures1/index.html
    * People note that the two-lock queue is incorrect! It doesn't
      have the Michael/Scott trick.
* Building a (half-assed) Treiber stack
    * http://www.ibm.com/developerworks/aix/library/au-multithreaded_structures2/index.html
    * I have a lot more detail than this...
* Michael and Scott paper
    * https://www.research.ibm.com/people/m/michael/podc-1996.pdf
* https://aturon.github.io/blog/2015/08/27/epoch/
    * Had good info about epochs.
* https://wiki.eecs.yorku.ca/course_archive/2007-08/F/6490A/_media/presentations:a2pres.ppt
    * Two lock queue powerpoint with images!
* https://www.youtube.com/watch?v=HJ-719EGIts&feature=youtu.be
    * Cliff Click describes Azul's version of a concurrent HM. It's
      lock free and does resizing.
    * A lot about memory fencing.
    * I have an email thread where Doug Lea is considering updating
      the CHM to either Click's or Shalev-Shavit. So those do seem
      like two leading contenders.

## More Books

* bought these.
* http://www.amazon.com/Art-Multiprocessor-Programming-Revised-Reprint/dp/0123973376/ref=sr_1_1?ie=UTF8&qid=1454457397&sr=8-1&keywords=The+Art+of+Multiprocessor+Programming
* http://www.amazon.com/C-Concurrency-Action-Practical-Multithreading/dp/1933988770/ref=pd_sim_14_2?ie=UTF8&dpID=51nuLYxU2iL&dpSrc=sims&preST=_AC_UL160_SR128%2C160_&refRID=0YEA0ZVEF4BAN3S843WM
