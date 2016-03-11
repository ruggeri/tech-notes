## Pointer Tagging

Normally a variable in a dynamic language stores a reference (i.e.,
pointer) to a memory location where the object lives. The object needs
to store its type.

This is annoying for primitive types, which then have to get allocated
on the heap. They're also subject to garbage collection too.

Therefore, we use the fact that all objects will be aligned to, say, a
4byte boundary. That leaves a couple bits that will always be zero in
a pointer.

You use one of these bits to mean "I'm an integer, not a
pointer". The other 63 bits can store the integer.

Such an object is often called `Fixnum` in Ruby and Lisp. Note that it
has a range befitting a 63bit quantity.

## Integer Promotion

For 64bit ints, you could a reference to a word storing the integer
value. However, you typically jump right to a multi-word bignum
representation, which is of course slow.

The language should do the promotion once you roll past the boundary,
meaning math is always correct.

This means every fixnum operation must involve a rangecheck. Also, a
call site returning a fixnum today could return a bignum tomorrow,
creating some complexity for optimization.

Ruby represents bignums with a length and a pointer to a series of
contiguous 32bit blocks. The first block represents bytes 0-32, the
next 33-64, etc.

Bignums are their own object, sadly. However, for bignums that are
small (prolly numbers needing exactly 64bits), it can reuse the
pointer to the block representation as an integer.
