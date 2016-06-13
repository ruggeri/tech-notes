It sounds like the major benefit of column-stores is the very high
compression on the columns. This is because you can keep things in
sorted order.

As everyone says, OLTP is not where c-stores shine, because you must
reconstruct the rows. Table scans see the most benefit when you only
need one or a few columns. Basically: if your columns are sparse or
low-cardinality, you can see major benefit.

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
