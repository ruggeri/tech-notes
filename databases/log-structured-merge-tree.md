SSD is still block orriented, and not nearly as fast as RAM still, so
B-Tree can still makes sense. Random reads can be fast, but not random
writes on SSD. SSD doesn't like to do random overwrites, it likes to
append. This is because SSD cannot directly overwrite; they need to
erase before writing, but erase happens in large "blocks", while write
happens in smaller "pages".

For that reason, appending is good, because you erase a block and just
keep extending.

## Copy-On-Write Trees

Write to an append log your changes, in memory reflect the updates in
your b-tree blocks. This allows your random writes. Also gives good
failure modes for a file-system (journaling). Very similar to DB stuff.

This avoids random IO and does more sequentially. The log presumably
gets compacted from time-to-time.

I'm not sure if I'm missing something here...

## Log-Structured Merge Trees

You have a tree in memory. Writes hit memory, and are written in a
log. The log is, from time-to-time, compacted and a new tree structure
is written on disk.

I believe that the disk tree is much more compact, for one. That's
because it's not experiencing a high load of inserts.

Also, the disk tree is updated in a smart way. You pick a big chunk of
the memory tree, and merge this out into the disk tree. That is much
more efficient than random insertions.

## Sources

* http://paperhub.s3.amazonaws.com/18e91eb4db2114a06ea614f0384f2784.pdf
