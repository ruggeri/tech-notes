I can only think of a few advantages of linked lists.

* Worst case `O(1)` push and pop. Dynamic arrays ammortize that.
* No advantage for `select!` as you can do in `O(n)` time and `O(1)`
  space for a dynarray.
    * Does recover space as you delete.
* Persistent data structure. Easier to make into a concurrent queue.
    * If you don't need concurrent, can just do a ring buffer.
    * In fact, if you only have one thread on each side, or if you're
      okay with locks, you can do this with a ring buffer.
    * But if you want multiple threads on each side without locking,
      you want a linked list.
* In fact, comes up a lot in concurrent algos.
* With hints can be fast for random access removal. LRUCache is a good
  example.
    * Keep a LL of elements from most recently access to least.
    * Also keep a hash map to the links, so you can move them around
      on access.
