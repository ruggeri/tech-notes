# Sorting

## The Bad Sorts

* Insertion sort
    * Work proportional to number of inversions; good for mostly
      sorted data.
    * Online; can sort data whilst it is being received.
* Selection sort
    * Can be shown to be constant factor slower than insertion
      typically?
* Shell sort
    * Repeats insertion sort for strided subarrays. Not sure why this
      might be superior.
* Bubble sort
    * Apparently just terrible.

All these have similar `O(n**2)` behavior.

## The Good Sorts

* Mergesort (von Neumann)
* Heapsort (JWJ Williams; who that?)
* Quicksort (Tony Hoare)

Of these, the first two are `O(nlog(n))`, while for quicksort this is
the expected running time (worst case `O(n**2)`). There are several
dimensions of consideration.

**Stability**: only merge sort is stable.

**Extra memory**: mergesort uses `O(n)` extra memory, quicksort
`O(log(n))` stack space, and heapsort in place (`O(1)`).

In the case of quicksort, all allocation is on the call stack.

Let's see that you can do mergesort with `O(n)` memory. Inductively
this makes sense. Sort both halves. The result returns two arrays with
total size summing to `n`. Then do a merge into a third array of
length `n`.

**Cache Performance**: preferred that one does not jump around very
much. Heapsort has bad cache performance because long jumps in
heapify-up/heapify-down. Mergesort has better cache performance
because it just slides through the two sorted halves and
merges.

Quicksort does do some jumping when it partitions, but not nearly so
much as in heapsort, because each successive partition reduces the
space to jump in by half.

**External Sorting**: We've discussed how memory caching architecture
gives cache-friendly algorithms better performance. But what if we
want to sort more data than can fit in main memory?

Note that since heapsort turns the whole dataset into a heap, it
cannot fit that datastructure into memory. This means a lot of disk
accesses.

Likewise quicksort requires partitioning. Trivial implementation would
require many seeks.

Polyphase merge sort is a classic solution. Do a streaming read of a
block of the file, reading as much as possible into memory. Next do an
in memory sort. Flush back to disk. Repeat for the other blocks.

Now you need to merge. For each block, read in the front. Leave some
memory for an output buffer. Begin merging. When the output buffer is
full, flush to disk. When an input buffer is empty, read to fill it.

## Hybrids

Introsort starts by using quicksort, but if bad luck is had with
pivots, begins using heapsort.
