Fractal tree, like a B-tree, keeps data in-order, and has `log_B(N)`
lookup, where `B` is a branching factor. However, Fractal-trees have
asymptotically faster insertion/deletion.

Like a B-tree, you have keys stored in nodes, if there are `B-1` keys,
then this node has a branching factor of `B`. Same key-order layout.

Fractal trees, unlike B-trees, use a smaller `B` than the blocksize
would allow. They do this, because they want to use some of the block
for other purposes. So Fractal-trees will have a smaller branching
factor. However, they choose a branching factor that still results in
logarithmic search time. For instance, if the blocks size is `S`, they
will set `B=\sqrt{S}`. Then the search time is
`O(log_{\sqrt{S}}(N))=O(log_S(N))`. So we're not terribly
underutilizing the block for branching. The search time will be a
constant amount slower, even as the blocksize increases.



B-trees can become fragmented, where the blocks are ~50% full. This
clearly wastes space, but it also causes extra disk access.
