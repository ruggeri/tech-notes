In C++, a vector is basically a single sided dynamic array. It has
amortized O(1) push_back; O(n) push at the front. You can use it as a
C-style array; that's required by the standard. However, resizes will
invalidate old references to elements (they may be moved).

A C++ deque extends an array by allowing O(1) amortized push at the
front. We could do this using a ring buffer. However, the C++ deque,
for unknown reasons, wants to preserve references even while
adding/removing elements (required by standard). You could store just
pointers in the ring buffer, but that would add extra indirection. For
this reason, it is more common to use an array of fixed-size arrays of
elements.

Of course, you could use a list for the deque, but that wouldn't have
support O(1) random access.

As a downside, you can't use a deque as a C-array.

It is unclear whether a deque is required to have worst-case O(1)
push/pop. If so, it seems hard to reconcile that with O(1) random
access, since the natural choices seem to be LL or ring-buffer.

Source: http://stackoverflow.com/questions/8627373/what-data-structure-exactly-are-deques-in-c

To get this to work, it looks like deques can use the lazy copying
trick. A 2nd array of double the capacity is always kept, and items
are copied into there. According to this paper, the SGI STL doesn't do
this; they claim that's a violation of the standard.

Source: http://www.cphstl.dk/Report/Deque-first-attempt/cphstl-report-2001-4.pdf
