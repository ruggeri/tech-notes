## Binary Heap

Of course, I know the binary heap well. Main operations:

* Find min: `O(1)`.
* Insert: `O(log(n))`.
    * But expected `O(1)`.
* Delete min: `O(log(n))`.
* Decrease key: `O(log(n))`.
    * If you put a hash map in front.

The array representation is well known to me.

## Binary Heap vs Self-Balancing BST

* Expected insertion time is `O(1)` in a Heap!
    * Because each level is bigger numbers.
    * And that's before you go binomial or fibonacci.
    * For a BST, it's `O(log n)`.
    * Note that this logic *does not* apply for extraction.
* Efficient implementation in Array.
* Heap creation from `n` elements is `O(n)`.
    * Put everything in array.
    * Bottom-most level consists of 1-el subtrees already heapified.
    * Consider subtrees rooted in 2nd to bottom level; both the left
      and right subtrees respect heap properties, so just heapify the
      root down.
    * Repeat upward.
    * Each level there are half the subtrees, but the work you're
      doing per subtree is increasing linearly.
    * Therefore the sum of the work is dominated by a linear function.

## Array Implementation?

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

## Binomial Trees/Heaps

The goal of the binomial heap is to provide fast amortized insert.

A binomial heap is a collection of **binomial trees**. Binomial trees
are recursively defined as:

* The tree of one element ("order 0"),
* The "order n" tree consists of a root, where all children are,
  left-to-right, the trees of order "0" up through "order n-1".

A binomial heap consists of a series of binomial trees, all of which:

* Obey the heap property.
* There is either one or zero of each size heap.

**Lemma**: There are at most `log(n)` trees in the heap. This is
because since binomial heaps have size of `1, 2, 4 ...`, since they're
always the sum of all prior sizes, plus one. Basically, this is kind
of "binary" in the sense that each tree holds `2**i` elements. So the
number of trees in the heap is popcount of the binary representation.

Merging two binomial *trees* of the same order is trivially `O(1)`:
you keep the tree with the smaller root, and affix the other tree as a
subtree.

Insertion into the heap operates as a merge with the heap of one
element. This can take `O(log(n))` time. Consider if there are
`2**k-1` elements in the heap; then there are `k` trees in the
heap. Adding a single element causes the first tree to merge with the
new element, which needs to merge with the second tree, which needs to
merge... Each rollover takes `O(1)` time, and there are `log(n)`
trees, so this is `O(log(n))` time overall.

Let's consider the insertion on an amortized basis. The rollover
clears out all the small trees, which means we'll next do a rollover
this bad in `2**k` inserts. Thus the cost amortizes out to `O(1)`. A
little hand-wavy, but should give the idea.

Note that whenever adding a new element, you will have to update the
min for finding the minimum, which can be done in `O(1)` time by
comparing to the current minimum.

Removal means cutting a root, considering its children as another
binomial heap, and merging its subtrees with the heap's other
subtrees. The worst case is to pick the highest order tree, which has
the most children. Consider if there are `2**k - 1` elements; then the
largest binomial tree stores `2**(k-1)` elements; in particular, its
root has `k-1` children.

Each of these children must be merged in. If we merge the largest
subtree first, there can be at most one rollover; `O(1)` time. This is
true by induction for each subsequent subtree. Therefore, the overall
time of the cut is `O(log(n))`.

Note that the cut very rarely reduces the size of the largest tree
(the high order bit in the count), so we won't see the cost ammortize
out.

Decreasing a key is still `O(log(n))`, because you have to bubble
up. The worst case is that the decreased key lives in the largest
tree, which is of depth `O(log(n))`. This of course assumes a hash map
in front.

* Find min: `O(1)`.
* Insert: `O(log(n))` WC, `O(1)` amortized.
    * **This improved!** Not just expected `O(1)` anymore!
* Delete min: `O(log(n))`.
* Decrease key: `O(log(n))`.

**NB: Called a Binomial Heap because there are `n choose d` nodes at
depth `d` in an order `n` binomial heap.** Not particularly exciting,
but helps explain the name.

Merge time is also logarithmic in the size of the larger heap. That
could be helpful for algorihtms that are exploring from different
points than want to "link up".

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

* Cut and remove the minimum that you saved.
* Add all its children as trees of the heap.
* *Compact the trees. Any two trees with equal degree are merged; one
  becomes the child of the other.*
* We then iterate through the remaining trees to calculate the new
  minimum.

At this point, note that the trees we are dealing with (so far!) are
binomial trees. The order 0 fibonacci trees have 1 element, the order
1 have a single order 0 subtree. The order 2 subtrees have an order 1
subtree and an order 0 subtree. And likewise.

All we've done is made our compaction *lazy*. Instead of doing `O(1)`
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
to talk about the *potential* of the heap. This is a measure of the
disorganization of the tree.

The potential will be defined as `t(H)+2m(H)`, where `t(H)` is the
number of trees in the list, and `m(H)` is the number of *marked
nodes*. Nodes are marked when they lose a child due to a decrease-key
operation. A node is "unmarked" when it is added as a child of another
node (e.g., when compaction occurs).

Let's consider the various operations:

* Find min is `O(1)`; it doesn't change the potential. This is
  constant time.
* Insertion is `O(1)`, but also increases the potential by a constant
  amount `O(1)`. It is still, in the amortized analysis, constant
  time.
* Merge involves the concatenation of two root lists. This is
  O(1). The overall potential does not change.
* Extraction is `O(1)` to cut the min-root and merge its children onto
  the root list. But then increases the potential by 1 for each child
  of the min root appended to the root list. Also, we need to look at
  all trees to find the new minimum; this is the "consolidation" phase.
    * To consolidate, we merge root trees with equal root degree.
    * The easiest way to do this is to create an array that indexes
      root trees by root degree. Iterate through, adding root trees to
      this. Merge as necessary, moving up to the next tree.
    * Then we run through the consolidated tree, finding the new
      minimum, and turning the array to a LL.
    * I claim that the consolidation is linear in the size of the root
      set after cutting the min root. We add each item into the
      array. We may have to roll over; but that means there is one
      less item in the array. Let's put it another way: for every two
      trees that combine, one tree is eliminated. The maximum number
      of combines is therefore `num_roots-1`.
    * This is linear in the overall number of trees.
    * We'll show (later) that the maximum degree of a root is always
      `log(n)`. Likewise, that means that there are at most `log(n)`
      roots *after* consolidation.
    * Thus, we create `log(n)` work to consolidate with our cut of the
      min, then we do the consolidation work, which cleans up all but
      `log(n)` trees, any of which was excess potential. Therefore, we
      should only be billed `log(n)` work!
    * Basically, we only create `log(n)` work; the rest is
      cleanup. Cleanup time is proportional to the amount of mess that
      has been made.
* To decrease a key, we cut it and add it to the list. This is `O(1)`
  and increases the number of trees by 1.
    * Except it also increases the potential by reducing the children
      in the parent. If parents had too few children, then we would
      not be able to ensure the maximum branching factor was `log(n)`.
    * We'll do a "cascading cut" up the tree; removing the parent if
      it has lost two children. Obviously this could go all the way up
      the tree (`O(log(n))` work).
    * Notice that each time we remove a marked node and add it to the
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
