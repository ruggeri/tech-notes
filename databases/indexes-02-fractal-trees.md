Like a B-tree, fractal trees keeps data in-order, and has `log_B(N)`
lookup, where `B` is a branching factor. However, fractal trees have
asymptotically faster insertion/deletion.

Like a B-tree, you have keys stored in nodes, if there are `B-1` keys,
then this node has a branching factor of `B`. Same key-order layout.

Fractal trees, unlike B-trees, use a smaller `B` than the blocksize
would allow. They do this, because they want to use some of the block
for other purposes. So fractal trees will have a smaller branching
factor.

However, they choose a branching factor that still results in
logarithmic search time. For instance, if the blocks size is `S`, they
may set `B=\sqrt{S}`. Then the search time is
`O(log_{\sqrt{S}}(N))=O(log_S(N))`. So we're not terribly
under-utilizing the block for branching. The search time will be a
constant amount slower, even as the blocksize increases.

The remaining space is used for buffering writes. When the buffer gets
filled, you pick the child with the most buffered writes, and flush
those to the child. If you picked `B=\sqrt{S}`, then that leaves
`S-\sqrt{S}` space for messages, which is still approximately `S`. For
each of the `\sqrt{S}` children, then, the smallest that the largest
number of messages for that child could be is `S/\sqrt{S}`.

So consider one insert. It needs to be flushed `O(\log_B{N})`
times. Because flushes are batched, each costs
`O(1/\sqrt{S})`. Therefore the cost of an insert is
`O(\log_B{N}/\sqrt{S})`.

## Comparison to B-Tree

Fractal tree will be deeper, by a constant amount. This is mitigated
if all (or nearly all) internal nodes can be cached in memory. Then
both fractal and b-tree might need typically one random IO each.

For inserts, the fractal tree can really shine, because of the factor
of `\sqrt{S}` in the denominator. Since `S` can be very large, the
sqrt can be a factor of `~100`, which is extremely significant.

OTOH, B-trees have excellent sequential insertion: `O(1/B)`, so
depending on workload, one might be better than the other.

## Comparison to LSM

Wikipedia claims that LSM has same write throughput, but worse read
(because of searching many trees). That makes sense: if there are
`O(log(N))` trees, and each takes `O(log(N))` time on average to
search, you're looking at `O(log(N)**2)`. Which is asymptoticaly
slower.

## Implementation

* Used by Tokutek.
* I don't see that they are used elsewhere. Maybe because the paper
  just came out in 2007?
