I can only think of a few advantages of linked lists.

- LL has worst case `O(1)` push and pop. Dynamic arrays amortize that.
  So dynarray is already a pretty good stack.
  - And you can write a singly- or doubly-ended queue with a resizable
    circular buffer. It will have amortized `O(1)` push/pop at
    front/back.
  - LL can be better if you worry about a worst case resize, though. It
    can be simpler to write and to teach.
- LL has no special advantage for `select!` as you can do in `O(n)` time
  and `O(1)` space for a dynarray.
  - The LL does recover space as you delete.
- LL is a persistent data structure. Easier to make into a concurrent
  queue.
  - If you don't need concurrent, can just do a ring buffer with a
    dynarray.
  - In fact, if you only have one thread on each side, or if you're
    okay with locks, you can do this with a ring buffer.
  - But if you want multiple threads on each side without locking,
    you want a linked list.
- In fact, LL comes up a lot in concurrent algos.
- You can quickly insert or cut a single value from the list. But you
  need to get to the associated node first.
  - That's not a _common_ scenario, but it can happen.
  - An LRU Cache will have a LL, which will keep `(key, value)` pairs in
    order of most recently used.
  - It will also have a hash map from key to LL node.
  - On each access that hits, it will jump to node, cut it, and prefix
    it at head of LL.
  - On a cache miss, will cut tail of the LL, and delete from hash map.
