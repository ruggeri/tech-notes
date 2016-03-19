## Counting Sort

Given a range, you initialize an array of that length, and count the
number of occurrences of each value. Then you can build the sorted
array, with `k` copies of each key.

This has `O(N+n)` time complexity (to allocate and zero the array) and
`O(N)` space complexity, where `N` is the maximum value.

## Radix Sort

In LSD radix sort, you start with the least-significant bit, and move
sort everyone based on this bit. You do this in a stable way. You then
repeat for more significant bits. This takes `O(wn)` time, where `w`
is the word size.

I would tend to expect this to be faster than a comparison sort. But
you need to be able to bucket stuff, which keeps you from doing this
in place.

MSD radix sort works basically the same way, but you start with the
highest bit and work down. You need to sort further just inside
"buckets". This is much easier to do in-place, at the loss of
stability. Presumably the MSD version has better cache locality.

The cost of MSD is that it uses logarithmic stack space. But that
doesn't seem very bad, honestly.

## Bucket Sort

This is really just the same as MSD radix sort; you partition into
bins, and then sort the bins. In MSD radix sort, those bins are just
powers of two.

I think there is some kind of distinction between bucket and MSD radix
sort, but it's stupid, whatever it is.
