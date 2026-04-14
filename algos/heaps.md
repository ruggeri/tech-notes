## Binary Heap

Of course, I know the binary heap well. Main operations:

- Find min: `O(1)`.
  - It's the root of the tree.
- Insert: `O(log(n))`.
  - But expected `O(1)`.
  - Just involves a heapify-up. Easy to append to an array of nodes,
    which puts element at leftmost open position in next unfilled level.
- Delete min: `O(log(n))`.
  - You're extracting the root of the key.
  - You cut the most rightmost node in the last level and place it at
    top. You then heapify-down from the root, swapping elements.
- Decrease/increase key: `O(log(n))`.
  - If you put a hash map in front, you can jump to the middle of the
    heap.
  - Then you heapify-up from there, to bring the decreased key to the
    root.
  - Conversely, you can increase-key, which involves heapify-down in a
    min heap.
- These are the operations of a priority queue

In the array implementation, you don't store any pointers. The children
of `idx` live at `2*idx + 1, 2*idx + 2`. Why does this work? Because all
the pairs of children of nodes before `idx` must live _before_ the
children of node `idx`. That's `2*idx` children. Also, you must account
for the node that is the child of no one: the root. So `2*idx + 1`
nodes must precede the children of node `idx`.

## Binary Heap vs Self-Balancing BST

- Expected insertion time is `O(1)` in a Heap!
  - Because each level is bigger numbers.
  - And that's before you go binomial or fibonacci.
  - For a BST, it's `O(log n)`.
  - Note that this logic _does not_ apply for extraction.
- Efficient implementation in array. No pointer storage or chasing.
  Probably really friendly for prediction.
- Heap creation from `n` elements is `O(n)`.
  - Put everything in array.
  - Bottom-most level consists of 1-el subtrees already heapified.
  - Consider subtrees rooted in 2nd to bottom level; both the left
    and right subtrees respect heap properties, so just heapify the
    root down.
  - Repeat upward.
  - Each level there are half the subtrees, but the work you're
    doing per subtree is increasing linearly.
  - Therefore the sum of the work is dominated by a linear function.

## Array Implementation Resizing

A common approach is to build a heap with an array, in which case
inserting into/removing from the tail position is O(1).

This means that insert is actually amortized, expected time O(1). Can
we eliminate the amortization? One way is to keep a doubly-linked list
at each level. You can then keep a cursor into the previous filled
level. Each time you have added a second child, you move on to the
next item. When you are done, you start iterating through the new
level. This also allows fast removal.

I think you can also just use clever math to find the right
position. Finally, you can use a dynamic array with O(1) WC insert
that does the lazy copying.

I think this is probably silly, because as soon as you add pointers you
probably make things slower...

## Binomial Trees/Heaps

The goal of the binomial heap is to provide fast amortized insert.

A binomial heap is a collection of **binomial trees**. Binomial trees
are recursively defined as:

- The tree of one element ("order 0"),
- The "order n" tree consists of a root, where all children are,
  left-to-right, the trees of order "0" up through "order n-1".
- An order `n` tree contains `2**n` elements.

A binomial heap consists of a list of binomial trees, all of which:

- Obey the heap property.
- There is either one or zero of each size heap.

**Lemma**: There are at most `log(n)` trees in the heap. This is
because since binomial heaps have size of `1, 2, 4 ...`, since they're
always the sum of all prior sizes, plus one. Basically, this is kind
of "binary" in the sense that each tree holds `2**i` elements. So the
number of trees in the heap is popcount of the binary representation.

Merging two binomial _trees_ of the same order `i` is trivially `O(1)`:
you keep the tree with the smaller root, and affix the other tree as a
subtree. You now have an order `i+1` binomial tree. Also, the heap
property is still maintained (parents have priority key less than
children).

Now, to insert an element into the heap, you make a new binomial tree of
order zero, with just the element. You will now merge this into the
binomial heap (list of binomial trees). If there was no order zero tree
in the heap, now there is. But if there was one, you must merge the two
trees, to create an order one binomial tree. Now merge this into the
heap.

This can take `O(log(n))` time in the worst case, because there are up
to `log(n)` binomial trees in the heap. Consider if there are trees of
order zero through `k` in the heap: there are `2**k - 1` elements.
Adding a single element causes the first tree to merge with the new
element, which needs to merge with the second tree, which needs to
merge... Each rollover takes `O(1)` time, and there are `log(n)` trees,
so this is `O(log(n))` time overall.

Let's consider the insertion on an amortized basis. The rollover
clears out all the small trees, which means we'll next do a rollover
this bad in `2**k` inserts. Thus the cost amortizes out to `O(1)`. A
little hand-wavy, but should give the idea.

Note that whenever adding a new element, you will have to update the min
tree for finding the minimum, which can be done in `O(1)` time by
comparing to the current minimum.

Removal means cutting a root, considering its children as another
binomial heap, and merging its subtrees with the heap's other subtrees.
The worst case is that we must cut the root of the highest order tree,
which has the most children. In that case, we may have to do the most
merging.

So consider if the heap contains trees of order zero through `k`. Then
there are `2**k - 1` elements in the binomial heap. The order `k`
binomial tree stores `2**(k-1)` elements, and has `k` children (trees
from order zero to `k-1`).

We will remove the root, but each of the child trees must be merged back
in. If we merge the largest subtree (order `k-1`) first, there can be at
most one rollover; `O(1)` time. That's because the heap might already
have an order `k-1` tree already, but we _know_ it does not have an
order `k` tree, because that's the one we've cut (and are re-adding
children of).

By induction, merge of each subsequently smaller subtree of the cut tree
takes `O(1)` time. Therefore, the overall time of the cut is
`O(log(n))`.

Note that the cut very rarely reduces the size of the largest tree (the
high order bit in the count), so we won't see the cost amortize out.

Decreasing a key is still `O(log(n))`, because you have to bubble
up. The worst case is that the decreased key lives in the largest
tree, which is of depth `O(log(n))`. This of course assumes a hash map
in front.

- Find min: `O(1)`.
- Insert: `O(log(n))` WC, `O(1)` amortized.
  - **This improved!** Not just expected `O(1)` anymore!
- Delete min: `O(log(n))`.
- Decrease key: `O(log(n))`.

You sort-of pay a lot to "speed up" insert. You now store actual
pointers, which will probably kill locality of reference. You didn't
really speed up the insert except in the sense of being able to tolerate
adversarial insert. But this kind of heap might be worthwhile if you
need to use something tree-based because you want a persistent
datastructure, and you can't be mutating arrays...

Merge time is also logarithmic in the size of the larger heap. That
could be helpful for algorithms that are exploring from different points
than want to "link up".

**NB: Called a Binomial Heap because there are `n choose d` nodes at
depth `d` in an order `n` binomial heap.** Not particularly exciting,
but helps explain the name.

## Fibonacci Heap

The goal of Fibonacci Heap is to get `O(1)` amortized reduce key. It
will also give us true `O(1)` insert (worst case, not expected, not
amortized). The downside is amortized `O(log(n))` extract; this used
to be a worst-case bound.

Notice, that the Fibonacci heap is likely to be useful if we are
calling reduce-key a lot. That can happen in graph algorithms where we
call reduce-key once per edge.

We keep a linked list of trees, each obeying the heap property. We
always maintain a pointer to the root containing the minimum
element. Notice this is a looser structure thus far from Binomial
Heaps: no restriction on structure of the trees yet.

We immediately have that `findMin` (i.e., `peek`) is `O(1)`. Merging
two heaps is `O(1)` by appending to the doubly-linked list. Updating
the minimum element may be necessary. Insertion is `O(1)` (worst-case)
by creating a new tree of size 1 and adding it to the linked list.

Because we haven't been diligent about combining trees as in Binomial
Heap, we have a potential problem with our delete min. The steps of
extraction are:

- Cut and remove the minimum that you saved.
- Add all its children as trees of the heap.
- _Compact the trees. Any two trees with equal degree are merged; one
  becomes the child of the other._
- We then iterate through the remaining trees to calculate the new
  minimum.

At this point, note that the trees we are dealing with (so far!) are
binomial trees. The order 0 fibonacci trees have 1 element, the order
1 have a single order 0 subtree. The order 2 subtrees have an order 1
subtree and an order 0 subtree. And likewise.

All we've done is made our compaction _lazy_. Instead of doing `O(1)`
amortized work when inserting, we're doing WC `O(1)`. Then we do
`O(n)` compaction work when we extract next. But when you amortize
this cost out across all the inserts you did, you're doing like `O(1)`
extra work in extract per previous insert. And of course future
extractions are fast.

**Decrease Key**

This is where it gets exciting. When we decrease a key, if it violates
the heap property, instead of bubbling up, we cut the subtree and push
it onto the list of trees. This is `O(1)` (append a LL of children to
the roots list); the idea is that we'll create more trees, but then
we'll clean this all up on extraction.

By cutting out nodes, are trees will not stay binomial. This can hurt
the bounds given above.

**Potential Method**

For the purpose of reasoning about the amortized analysis, we're going
to talk about the _potential_ of the heap. This is a measure of the
disorganization of the tree.

The potential will be defined as `t(H)+2m(H)`, where `t(H)` is the
number of trees in the list, and `m(H)` is the number of _marked
nodes_. Nodes are marked when they lose a child due to a decrease-key
operation. A node is "unmarked" when it is added as a child of another
node (e.g., when compaction occurs).

Let's consider the various operations:

- Find min is `O(1)`; it doesn't change the potential. This is
  constant time.
- Insertion is `O(1)`, but also increases the potential by a constant
  amount `O(1)`. It is still, in the amortized analysis, constant
  time.
- Merge involves the concatenation of two root lists. This is
  O(1). The overall potential does not change.
- Extraction is `O(1)` to cut the min-root and merge its children onto
  the root list. But that increases the potential by 1 for each child of
  the min root appended to the root list. Also, we need to look at all
  trees to find the new minimum; this is the "consolidation" phase.
  - To consolidate, we merge root trees with equal root degree.
  - The easiest way to do this is to create an array that indexes
    root trees by root degree. Iterate through, adding root trees to
    this. Merge as necessary, moving up to the next tree.
  - Then we run through the consolidated tree, finding the new
    minimum, and turning the array to a LL.
  - I claim that the consolidation is linear in the size of the root
    set after cutting the min root. We add each item into the
    array. We may have to roll over; but that means there is one
    less item in the array. Let's put it another way: for every two
    trees that combine, one tree is eliminated. The maximum number
    of combines is therefore `num_roots-1`.
  - This is linear in the overall number of trees.
  - We'll show (later) that the maximum degree of a root is always
    `log(n)`. Likewise, that means that there are at most `log(n)`
    roots _after_ consolidation.
  - Thus, we create `log(n)` work to consolidate with our cut of the
    min, then we do the consolidation work, which cleans up all but
    `log(n)` trees, any of which was excess potential. Therefore, we
    should only be billed `log(n)` work!
  - Basically, we only create `log(n)` work; the rest is
    cleanup. Cleanup time is proportional to the amount of mess that
    has been made.
- To decrease a key, we cut it and add it to the list. This is `O(1)`
  and increases the number of trees by 1.
  - Except it also increases the potential by reducing the children
    in the parent. If parents had too few children, then we would
    not be able to ensure the maximum branching factor was `log(n)`.
  - We'll do a "cascading cut" up the tree; removing the parent if
    it has lost two children. Obviously this could go all the way up
    the tree (`O(log(n))` work).
  - Notice that each time we remove a marked node and add it to the
    root list, we're actually starting some cleanup work. So we
    actually shouldn't be charged for this work; that should be
    charged toward the person who marked that node in the first
    place.

It only remains to prove that the maximum degree of a node is `log(n)`
when you only let one node be removed. You prove this assuming a
"maximally damaged tree", where every node has one child removed.

I'm lazy and won't do this today. But basically it's a very similar
argument to the binomial heap. You combine roots just as the binomial
heap; the only difference is that you might damage them! That removes
one node per child. Basically, I'd consider a binomial heap where
every element is one smaller than it should be. Then the number of
elements in such a tree is still exponential in the degree of the
tree. Thus the max degree must be logarithmic.

## Importance

To my knowledge, mostly in Dijkstra's algorithm to maintain a
priority queue that updates fast. Helps for dense graphs.
