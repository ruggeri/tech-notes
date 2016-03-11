* I know AVL tree. Keeps tree balanced, which means depth is
  logarithmic.
* Can do either one or two rotations about a node to fix it up.
* Can be done in a persistent way.

Why not a hash map?

* Worst case time bound. More interactive.
* Persistent data structure.
* Allows `O(n)` in order traversal.
* Very efficient for range queries, which happen in databases.
* Can be used as a double-ended priority queue.
    * could also use two heaps as a doubly-ended priority queue.

# B-Trees

Has very high branching factor. At maximum it stores, say, 31 keys,
and has 32 children. Idea is that each node is equal to size of a disk
page. These are designed for secondary storage performance.

All leaves occur at the same level. An interior node with `k` values
must have `k+1` children; a leaf node will have no children. The
values are stored in sorted order. Each child pointer is "between" two
values; those window the values in the branch.

B-Trees respect the property that the minimum number of keysis `t-1`,
while the maximum number is `2t-1`. So, for instance, 15 keys minimum
and 31 keys maximum. This corresponds to between `t` and `2t`
children. The only possible exception is the root.

So that's the structure. This immediately implies that a size is at
most logarithmic.

**Searching**

How to search for a key is obvious.

**Insertion**

You obviously follow down toward the node where a key belongs. If the
node is not full you just place it there.

If it is, then you are wanting to store `2t` values in a node, which
is more than the limit. So you pick a median value, and put `t`
elements in a node on one side and `t-1` elements on the other. You
then want to push the median value up into the parent, and place the
two nodes on either side.

Of course, what if there is no space in *that* interior node?! Then
that needs to be split, and a key moved up. If the root itself is
full, then the root must be split, and a single value is moved up into
a new root. The root may contain a single item.

Just to be totally clear... When you split an interior node, you're
going to put `t-1` keys on one side, and `t` on the other. `t`
children will go on one side, and `t+1` on the other. A single key
will move up toward the parent.

This clearly is logarithmic time.

To avoid bubbling up/down the tree, a recommended approach is to split
*all* full nodes on the way down, pre-emptively. That way there's no
bubbling. This allows insert in one pass.

Again, to be totally clear... You split pre-emptively, when there are
`2t-1` keys (`2t` children), you'll put `t-1` keys on the left (`t`
children) and `t-1` keys on the right `(t` children) and push one key
up. That works out perfectly.

**Deletion**

You travel down to the node where the key lives. You want to delete
it. If it is in a leaf, then remove it. If it is an interior node, go
to the left and get the biggest element less than the deleted key, or
the right and get the smallest element greater than the deleted
key. You can choose randomly. Move that up to the position of the key
you want to delete. This effectively deletes from a leaf node.

If the leaf node is still big enough, then done. If not, but a sibling
to the left or right has more than the minimum number of elements, do
a "rotation"; push down the separator, while pulling up the next
smaller/longer.

If both leaves were of minimum size before deletion, than one now
contains `t-2` elements and the other `t-1` elements. Combine them,
along with the key in the parent partitioning the two. The resulting
leaf has size `(t-2)+(t-1)+1=2t-2`.

But if both sibblings are too small, you need to merge with one of
them (you can choose randomly). Because `(t-1)+(t-1)=2t-2<2t-1`, this
node is not too big. However, you have removed a value from the
parent.

Let's consider rebalancing an interior node. Of course, you can do a
rotation: consider a rotation to the left. The parent key moves down
to the left child, while the right child's smallest key up to the
parent. A grandchild also shifts from the right child to the left
child. In that way, we increase *both* the number of keys in the left
and the number of children by one.

If both sibblings are both of minimum size, we merge. Note that the
merge pulls down the separating key in the parent.

Note that this approach descends and ascends the tree. CLRS seems to
suggest that there may be a way just to go down once.

**Deletion - One Pass**

The basic invariant is: we will only descend into a node when it has
at least `t` keys. We will structure our delete operations so that we
only descend once we have `t` keys in the child.

As before, if the key lives in a leaf node, delete it. We don't need
to merge because the leaf will have `>=t` keys.

If it is in an internal node, and the left child has at least `t` keys
(1 more than min), find the greatest key `k'` on the left and delete,
replacing `k` with `k'`. Symmetrically, if the right child child has
at least `t` keys (1 more than min), find the least key `k'` on the
right and delete, replacing `k` with `k'`. (Note that the following
explanation of how to avoid recursing into a child with `t-1` keys
should be used even when finding).

Otherwise, if both left and right have `t-1` children, then merge
them, pushing `k` down a level. Then recursively perform a delete for
`k` on this node.

If the interior node doesn't have the key, then consider the child to
recurse into. If the child has `>=t` keys, then continue. Else, we try
to do a rotation with a sibbling. Else we merge two nodes.

Note that we never have to climb up the tree, though we may have to
jump back to an interior node if the key to delete is found there.

## Sparse Index

Imagine searching a really big file on disk. You'd just jump around,
reading a bunch of blocks. What if, instead, you used a sparse index,
which recorded the first record in each block? Then you do bsearch in
the sparse index, and then only need to read the one block your value
is in and bsearch that one last block. You can keep layering these
until you have a one block index.

But if you're using sorted files, you're asking for trouble on
insertions. So that's why b-trees are used here. Obviously, this is
mostly for databases.

## B+ Tree

B+ tree is a tree where the data records are stored in the leaf nodes,
not interior nodes, which just store keys. This means that you can fit
more children per node. Also, the leaf nodes typically maintain a
linked-list, so that it's easy to do range scans. Otherwise range
scans involve moving up and down the tree.

OTOH, B trees allow the possibility for faster access to values closer
to the top.

## vEB Layout

Trees typically have pretty painful locality of reference, even when
packed into an array.

A van Emde Boas layout says to split the height of the tree in
half. You pack the first half van Emde Boas style (recursively), then
all the subtrees (of which there are many!) also van Emde Boas style
(recursively).

## TODO

* B-Star
* Concurrent B-Trees?
