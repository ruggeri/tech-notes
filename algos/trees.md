- I know AVL tree. Keeps tree balanced, which means depth is
  logarithmic.
- Can do either one or two rotations about a node to fix it up.
- Can be done in a persistent way.

Why not a hash map?

- Trees have better worst case time bound. More interactive.
- Trees are persistent data structures; hash map is not.
- Sorted trees allow `O(n)` in order traversal.
- BST is very efficient for range queries, which happen in databases.
- BST Can be used as a double-ended priority queue.
  - Could also use two heaps as a doubly-ended priority queue.
  - Though I think a better solution would just be two heaps, right?

## vEB Layout

Trees typically have pretty painful locality of reference, even when
packed into an array.

A van Emde Boas layout says to split the height of the tree in
half. You pack the first half van Emde Boas style (recursively), then
all the subtrees (of which there are many!) also van Emde Boas style
(recursively).

I believe I cover this better and more extensively elsewhere (esoteric
data structures document).
