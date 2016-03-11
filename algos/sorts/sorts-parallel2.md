**Note**: CLRS was very helpful.

There are a couple of ways to think about parallelization.

For a fixed problem size, you can find the relationship between number
of processors and runtime (called *speedup*: `T_1/T_p`). But you can't
endlessly add more processors and expect it to go faster and faster if
there is any serial component. That is Amdahl's law. So this is a
little silly.

Note that linear speedup means something special: you can get the
answer in less wall time, for the same amount of
processor-seconds. Your cost in processor-seconds is the same, but you
have flexibility over when the answer is delivered.

The *parallelism* of a problem is `T_1/T_\infty`. This can be thought
of as the average amount of work that can be done in parallel.

Likewise the *slackness* of a problem is `T_1/(p*T_\infty)` . If this
is greater than one, than we can acheive linear speedup by adding more
processors. If it is less than one, we cannot acheive linear speedup,
since there is not enough parallelism anymore. Basically, slackness is
a measure of how much we are utilizing each processor.

We can consider the asymptotic parallelism. If this tends toward
infinity, we can keep adding more processors to acheive greater
speedup. This is effectively Gustafson's law. That larger and larger
problem sizes will allow for a higher level of parallelism, allowing
more processors to be used at the same efficiency. This occurs if the
parallelizable work grows faster than the serial work.

Simple parallel merge sort, for instance, has a parallelism of `log
n`. That means you can acheive greater and greater speedups with more
and more processors, but that you need to double your problem size
before you can use another processor with the same efficiency.

It is possible that larger problems would enable better and better
opportunities for parallelism. But that is unlikely. However, it is
not entirely unreasonable to hope that the level of parallelism
possible might scale linearly in the size of the problem. This is, for
instance, true when building many buildings in a city; twice as many
buildings gives you twice the number of worksites to work on
simultaneously.

Typically, however, we would expect that, if we're breaking down
problems, we have to combine answers. That's what's slowing us down in
the parallel merge sort. But that's the point: if there were no
coordination of the solution to independent subproblems, the ideal
parallelism would be `O(n)`. How close can we get to that number?

In the case that `T_1` is `O(n)` *and* the parallelism is linear too,
then we can solve the problem in constant time for no increased
cost. But that is atypical, the time complexity of the serial verison
is likely to grow faster than the parallelism. But the worse the
parallelism is, the less more processors can help us solve larger and
larger problems.

## Merge Sort: No parallel merging

You might try to run the two halves of merge-sort in parallel, then
merge in the same serial way. If you do this, sorting is a linear time
process (recurrence is `T(n)=T(n/2)+O(n)`). That gives a logarithmic
parallelism.

That's not a good parallelism: even for large arrays, you can only
effectively use a couple processors, since this grows
logarithmically. Thus we want to *also* parallelize merging.

## Parallel Merging

So a silly attempt to parallelize merging (assuming we already sorted)
would be thus. Start with two processors. We calculate the median of
the left half in `O(1)` time (it's at the center position of the left
half). We can do binary search in the right half to locate the where
the median partitions the right half. That's `O(log n)` time.

We now know how many items are less than, and how many are more than,
the pivot. That is: we know where the pivot should be placed in the
merged array. Copy the pivot.

Recurse, performing this on the left side of the left and the left
side of the right, as well as the right of the left and the right of
the right.

If we perfectly partitioned, the depth of the recursion should be
`log(n)`. We know, that the median of one side is within `[1/4, 3/4]`
of the total data, so depth is asymptotically `log(n)` even in the
worst case.

It takes `log(n*2**-i)` time at each level to do the binary search. I
think, on average, this is `O(log(n))`. So the total work done is
`O((log(n))**2)`.

Note that this is *better* than `O(n)`.

## Returning to Merge Sort

So merging takes `O((log(n))**2)` time. There are still `log(n)`
levels of recursion. The recurrence for the time complexity is solved
by `O((log(n))**3)`. Again, I think that's because the average merging
work is still `O((log(n))**2)`.

This gives an overall parallelism of `O(n/(log(n))**2)`, which is
significantly better than `O(log(n))`. Basically, we're saying you can
add processors to help much more rapidly.

## Foundations

CLRS Source: https://mitpress.mit.edu/sites/default/files/titles/sample/0262533057chap27.pdf

**TODO3**: Discuss in-place merging.
**TODO3**: http://www.drdobbs.com/parallel/parallel-in-place-merge/240008783
