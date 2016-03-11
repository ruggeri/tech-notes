## Finger Trees

A finger tree is a functional data structure: it is a twist on a 2-3
tree (a tree where nodes have branching factor of 2 or 3, and all
values are in the last level).

The idea is you keep *fingers* to the front and back; fingers are
basically pointers to the interior of the tree so you can have fast
access.

Finger trees are used to implement other functional
datastructures. They could be used for a dequeue, though people say
the traditional Bankers' dequeue has better performance.

You can read more here, but I don't really care:

http://www.staff.city.ac.uk/~ross/papers/FingerTree.pdf

## Skip Lists

A linked list with several "express lanes". If the express lanes are
each twice as fast as the previous one, then lookup is `O(log(n))` in
a sorted list.

How to insert/remove? If you want to keep everything balanced, you
have to adjust all the lanes.

Randomize the structure. Flip a coin to be in the 2x lane. If so, then
flip a coin if you should be in the 4x lane. Etc. You can insert into
these lanes in `O(log(n))` time.

Likewise, you can create an indexable version if you store the
"widths" of each node. On insert/delete, it takes `O(log(n))` time to
find the previous nodes in each lane, updating the widths
appropriately.

Skip lists compete with self-balancing BSTs. They have a potential
advantage in distributed computing because they do not require global
locking; they can likely even be used in a lockfree way. But they have
poor data locality, which B-Trees addressed.

## Splay Tree

The idea here is that each time you access a node, via tree-rotations,
you bring it to the top. Ammortized, this gives `O(log n)` access
time; basically, frequently used stuff is at the top and fast to
access, and infrequently used may be farther. This is accomplished
*without* keeping any depth information.

The basic idea is to replace AVL with something which doesn't keep
track of that level of information.

A downside is that each *access* requires `O(log n)` tree
rotations. That can really hurt with paged memory, and is even worse
than AVL, which doesn't modify a tree when reading. This can hurt in
threaded environments.

Also, for a series of *non-random* accesses, splay trees can perform
better than other trees, since frequently used stuff is nearer to the
top.

## Treap

Basically, sort of a cross between a heap and a BST. You store entries
with priorities. It obeyes the BST property wrt to lookup of the
entries: you can compare at a node and know whether to go left or
right. It also obeys the heap property wrt the priorities: higher
priority stuff is parent to lower priority stuff.

Imagine insert. You go down to place the item in the BST. Then you
need to "heapify up". This is a little different though, since you
can't just switch places with a parent. Instead, do a tree rotation to
move the child up.

Notice that this means the treap may not have minimum height. But you
can show that the treap is equivalent to inserting nodes into a normal
BST in order of decreasing priority. Assuming the priority order is
random, this should have expected logarithmic height.

A typical use is to insert with random priorities. On each access of a
key, you can generate a new random priority; if it is greater, than
you update the priority and heapify up. This moves more frequently
accessed stuff up the tree. (My own thought: You could probably even
do some time decaying to make this more sensitive to recent use!).

Versus a splay tree: has *expected* (not ammortized) logarithmic
performance, but does fewer rotations. I think the rest of the
tradeoffs to an AVL still apply: modifies structure on reads, has less
bookkeeping, performs better given non-random access.

## van Emde Boas Trees

Most PQ structures have logarithmic extract. At least one of
insert/extract has to be logarithmic because of the bound on sorting.

But consider if the keys are not arbitrary, but from a range
`0...N`. Then we know we can sort this in linear time. So the argument
doesn't apply. So let's try to make a faster priority queue.

You could try to use a bitset, but that has linear
minimum/extract. You could try to impose a binary tree on top, with a
1 in a node if there is a present element underneath. But that sucks
because insert/extract are now logarithmic.

Let's say that instead of using a logarithmic heigh binary tree, we
use a branching factor of `sqrt(N)`. Then the height of the tree
is 2. In fact, let's just keep an array of length `sqrt(N)` above the
bitset, with a 1 if anyone has that value in the slice starting at
`i*sqrt(N)`.

Insert is now `O(1)`, but extract is `O(sqrt(N))`, which is worse than
before! Note this `sqrt(N)` comes at two levels. First, to find the
first slice with any bits set, we might have to go through `sqrt(N)`
slices. Second, when we're in that slice, we need to consider
`sqrt(N)` elements.

We have to go deeper. Let's assume `N=2**(2**k)`. Let's try to do
things recursively. A proto-vEB for a universe of size `N` has
`sqrt(N)` clusters to proto-vEBs for a universe of size `sqrt(N)`. It
also has its own summary of those clusters, itself a proto-vEB of size
`sqrt(N)`. The base case is `N=2`, which is just two bits.

To determine whether a value is in the set, we just go down the
clusters. Choosing the correct cluster to travel next to is `O(1)`,
and since each cluster is of size `N**(1/2**i)`, there are only
`O(log(log N))` steps. That's really fast!

But consider finding the minimum. We need to do a search in the
summary proto-vEB, just to find the right cluster that we then recurse
into. What's happening here is that even though our problem is going
down by a sqrt, we are also *doubling* our problems each time! We
can solve this:

```
T(N)=2T(sqrt(N))+O(1)
T(2**m)=2T(2**(m/2))+O(1)
T'(m)=2T'(m/2)+O(1)
```

So `T(2**m)=T'(m)=O(m)`. So `T(N)=O(log(N))`. The problem is that we
need to stop creating two new problems each time!

**Solution**

To prevent the double recursion, we store a `min` attribute in a
node. An important note is that we *will not* store the min in any
child. This is very important.

Peeking the min is `O(1)` obviously. Let's try to delete the min. The
first thing to do is find the cluster with the 2nd min. This is a peek
in the summary table! That's `O(1)`!

Now go to extract that min from the child. We'll store the min
here. We now go to update the child's min, repeating our procedure. If
the child is not empty after its min is pulled up, then this is a
single recursion and all is well.

In the case that this is the last element in the child, note that
we'll see this in the child's summary table in `O(1)`. We must update
*our* summary table. This is a matter of a delete in the summary
table. That's a single recursion.

BAM! Drop the mic.

There is a more complicated version that supports `O(log log(N)`
`find-next` and `maximum`. Basically you just track a `maximum`
too. But I'm lazy and won't do that right now.

## Other

* Unrolled LL.
* CDR Coding
* Gap Buffer
* Hashed Array Tree
* Judy Array
* Doubly Ended Priority Queue
