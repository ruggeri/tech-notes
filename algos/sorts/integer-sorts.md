## Counting Sort

Given a range, you initialize an array of that length, and count the
number of occurrences of each value. Then you can build the sorted
array, with `k` copies of each key.

This has `O(N+n)` time complexity (to allocate and zero the array) and
`O(N)` space complexity, where `N` is the maximum value.

# Least Significant Digit (LSD) Radix Sort

First, create an array of size `n` for temporary use.

In LSD radix sort, you start with the least significant digit (bit). You
are going to sort the array just based on this one bit.

You first go through and count the number of zero values and the number
of one values for that bit. Now you know how many of each value there
are. Call the number of one values `offset`

Go through again, and copy one-by-one the zero values to position `i` of
the temporary array, and the one values to `offset + j`. Increment `i`
and `j` as you copy elements.

This sorts by just the bit you're working on. It is _stable_. That is
essential, because we will repeat the sort on the next digit, but it is
essential that we not disturb our work sorting by smaller bits.

This makes `O(wn)` copies, and takes `O(wn)` time (`w` is the word
size). It takes `O(n)` extra memory. It doesn't recurse.

## Most Significant Digit (MSD) Radix Sort

MSD radix sort starts with the most significant bit. We will sort by
that bit, and then recurse on the two halves. This will take `O(log n)`
(stack) memory, but otherwise work in-place.

To work in-place, you must use the same kind of shuffling logic used
during partitioning in Quicksort. This is unstable, but it is what
allows us to avoid using a temporary array.

## Bucket Sort

Bucket sort is maybe a generalization of the MSD radix sort idea? You
distribute items to buckets, sort the buckets, and concatenate the
results.

It sounds a lot like MSD radix sort. However, the number of buckets
doesn't have to be a power of two. You can also choose a different
algorithm at a lower level of recursion.

Bucket sort appears to be more of a "concept" than a fully specified
sorting algorithm...
