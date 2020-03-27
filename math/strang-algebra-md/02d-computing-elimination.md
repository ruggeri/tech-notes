What is the speed of Gaussian elimination? Well, for each row, we need
to subtract it from every other row. This is `n(n-1)` row
operations. Each involves `n` elements. So we're talking `O(n**3)`
time.

This means that to 10x the dimension of the matrix, we 1,000x the time
to solve. But note that most matrices are sparse, so there are far
fewer operations to perform.

Another calculation note: sometimes Matlab will do row-exchanges for
the sake of numerical stability, since if a pivot is small, it can be
bad to scale by this.
