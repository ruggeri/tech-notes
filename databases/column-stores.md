Instead of storing in rows of `ID => columns data`, you store multiple
columns of `value => ids`. This makes building back a record quite a
bit harder, but if you only need one column, you can get great
compression (because sorting).

It sounds like the major benefit of column-stores is the very high
compression on the columns. This is because you can keep things in
sorted order.

As everyone says, OLTP is not where c-stores shine, because you must
reconstruct the rows. Table scans see the most benefit when you only
need one or a few columns. Basically: if your columns are sparse or
low-cardinality, you can see major benefit.

Reconstructing tuples is slow, so even if you need a few columns,
there is pain. You could imagine that the cost of joining pairs is
`N**2` reads. It's probably possible to do some organization, so that
corresponding blocks of the two columns will have corresponding
entries. Then after loading the two blocks, it's the CPU's job to join
the tuples by organizing the data. This trades off some compression
for much better disk perf. Note that the larger the gap between CPU
and disk, the more this tradeoff may make sense.

Here's a place to read more (Abadi, Design and Implementation of
Modern Column-Oriented Database Systems)

http://db.csail.mit.edu/pubs/abadi-column-stores.pdf

Looks like a monograph of ~70 pages.

MonetDB and C-Store are popular implementations.

There's a bunch about "OLAP" (online *analytic* processing) and
datacubes (basically time-series databases). But I don't want to get
into that now.

Source:

https://news.ycombinator.com/item?id=11896105
