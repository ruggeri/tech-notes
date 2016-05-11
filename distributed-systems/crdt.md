# Conflict-Free Replicated Data Type

Basic idea is that the same set of mutations, applied in any order,
will result in the same value.

I believe the idea is this: during a partition, keep writing. On heal,
merge the updates. The result is consistent with any ordering of those
updates.

In a sense, you haven't rolled back anything. There is no conflict.

On each side of the partition, you see a different order in which the
updates were applied; side A sees all the A operations, then the B
operations, while side B sees all the B operations, then the A
operations. So there isn't necessarily a globally consistent view of
the order of the transactions.

Note that CRDT never requires human intervention to resolve conflicts,
since there is no conflict on heal.

More thoughts:

* No synchronization needed
* More responsive, available, scalable.
* Handles latency, faults, disconnection.
* Does not require intervention, or demonstrate anomoly.
* Basically, Strong Eventual Consistency is proposed as an end-run
  around CAP.

Some examples:

**Counter**

For `n`-processes to share a counter, store `(c_1, ...,
c_n)`. Increment only your own. On merge, take max of all values.

**Increment/Decrement**

Keep two vectors: `(c_1, ..., c_n)` and `(d_1, ..., d_n)`; the first
records increments, the second decrements.

**Insertion Set**

Trivial; just keep adding elements to the set, and when merging take
the union.

You can use this with `(event, sequence number)` to have a partially
ordered set of events. I think you could even throw in a logical clock
here.

**Insertion/Removal Set**

Keep two sets, one for inserts, one for removes. Obviously you can
never (successfully) add after remove. If a remove happens
concurrently with an add, the remove comes last and wins.

There's a version called LWW-Element-Set, where we keep a list of
`(element, add time, delete time)`. We just choose the later of add
time and delete time. This would be exactly a set if there were a
global clock, otherwise it is a CRDT and pretty okay.

**Observed-Remove Set (OR-set)**

Keeps a list of `(element, [add times], [remove times])`. Element is
present if any add time is `[add times] - [remove times]` is
non-empty. To remove, you add set `remove times := remove times union
add times`.

**Graphs**

Paper shows how to do a directed graph. This would be (I think)
trivial if only adding.

**Co-operative Text Editing**

Here's the idea: we have two kinds of updates:

* Delete an element (character)
* Insert between two elements (a character between two characters).

To be correct, it must be a CRDT, and if we insert X between A and B,
then this must be respected in the final result.

One approach is to give characters identifiers inbetween existing
identifiers. For instance, if `id(A)=10` and `id(B)=15`, we might
create an update `insert(X, 12)`. Then everyone will put this betwee
the two. However, this means that our identifiers might get quite
long.

Note: you'll want to include a client ID to break ties, but let's
ignore that for now.

Their main focus is going to be: how do you allocate ids in such a way
that their length does not grow quickly. That would require
rebalancing, which could be costly.

Let's simplify: we're basically talking about a *tree*. The "base" of
the tree is the *arity*. The trick they use is this: every time you
add a new level to the tree, you double the base. Next what you do is
this: you need to add a new child; you can put it anywhere really; it
can be the first or the last child. They recommend putting it at a
random position in the first or last N children. The idea is to
support left-to-right editing, *or* right-to-left editing, without
having to to add too many levels.

Note now that removal is trivial. You go down that path in the tree
and put DELETED there. You can do that even if you haven't written it
yet!

Not really sure what happens if two clients want to write to the same
child? I think the answer is that every id is `(element id, site id)`,
to break ties. This does present the possibility of some text
appearing inside some other text, I think. But not in the long term,
since your cursors will end up in different places.

This is called LSEQ. It competes with Operational Transforms.

## Sources

* https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type
    * Kinda good.
* https://github.com/aphyr/meangirls
    * A few more examples of CRDT.
* https://hal.inria.fr/inria-00609399v1/document
    * Paper presenting the concept.
    * Mostly theoretical results; not hyper-interesting.
* http://hal.upmc.fr/file/index/docid/555588/filename/techreport.pdf
    * Presents a litany of CRDTs, including ones for co-operative text
      editing.
