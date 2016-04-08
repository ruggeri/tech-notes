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
    * Though I think a better solution would just be two heaps, right?

## vEB Layout

Trees typically have pretty painful locality of reference, even when
packed into an array.

A van Emde Boas layout says to split the height of the tree in
half. You pack the first half van Emde Boas style (recursively), then
all the subtrees (of which there are many!) also van Emde Boas style
(recursively).
