In C++, a vector is basically a single sided dynamic array. It has
amortized O(1) `push_back` and `pop_back`; O(n) `push_front` at the
front. You can use it as a C-style array; that's required by the
standard. However, resizes will invalidate old references to elements
(they may be moved).

A typical way to make a doubly-ended queue is with a "ring buffer" (has
a start index and a length). You can do this with a dynamic array, and
resize if the queue gets too long. You can't really use the buffer as a
C-style array anymore, since it may start in the middle of the backing
store. Of course, you can use a doubly linked list to implement a
doubly-ended queue, but it will have a lot of pointer chasing, and not
support O(1) random access.

The C++ `deque` in the standard library is an alternative to a C++
`vector`. It allows:

- Worst case O(1) `push_back` and `push_front`
- Worst case O(1) `pop_front` and `pop_back`.
- O(1) random access.
- Objects are not moved. References always remain valid.
- Not expected to act like a C array.

The solution is that `deque` is not a contiguous store. One idea would
be a linked list of smaller stores ("blocks"), but that would end up no
different than a LL: it wouldn't allow O(1) random access. And it won't
work to put blocks in a dynamic array; it would trigger resizes less
often, but they would still happen.

- Source: http://stackoverflow.com/questions/8627373/what-data-structure-exactly-are-deques-in-c
  - Not sure it's a super enlightening discussion.
- Source: http://www.cphstl.dk/Report/Deque-first-attempt/cphstl-report-2001-4.pdf
  - Link rotted, but still available on Wayback machine.
  - "The deque class in the Copenhagen STL: First attempt" by Bjarke
    Buur Mortensen.
  - They notice problem that push operations may not truly be O(1) if
    they trigger resize of block map.
  - Claims SGI STL uses a naive dynamic array for block map, and thus
    doesn't give O(1) push.
  - Suggests "deamortization" as a technique to ensure truly O(1) push.

## Deamortization of Dynamic Array Resize

Basic idea is that, when resize would be triggered, you don't copy _all_
items over. You copy two items at a time. To make this work, each
`push_back` would move two items from start of old store to start of new
store. You then use the available space at the front to temporarily
store the newly pushed back items.

Items will still be stored _mostly_ continguously, but not perfectly so.
You can still do O(1) random access. And of course there is some extra
bookkeeping on every step.

## Hash Map Buckets: Linked List?

Here, I think the idea behind using a linked list as a hash map bucket
is that you can have pointers into the hash map to the stored objects
and not invalidate those on resize of the bucket (or on rehashing for
that matter).

I think also a linked list might have lower overhead than a vector. Of
course you might have cache problems with a LL, but your list is only
going to have one item probably, right?

In the context of an LRU cache, it becomes O(1) to cut a link in a
bucket provided you have a reference to the link. But I don't see that
as a big win: again, isn't the list one item long? And anyway, you
remove a link in this way by adding an item. And adding an item will
require traversal of a bucket...
