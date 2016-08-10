* Typical B-Tree
    * Does Lehman/Yao thing for concurrent modifications to the B-Tree.
* GIN
    * Generalized Inverted Index
    * Optimized for postings list type stuff with lots of duplicate
      keys.
    * Rather than keeping a compressed posting list of row ids, they
      use a posting tree.
    * Also allow "fast-updates" by keeping a buffer of recent inserts
      into the index that will periodically be batch processed.
        * These need to be reviewed on every query.
    * You can query based on intersection.
        * The way this is done is to build a bitmap index.
        * Each row in the table gets a bit. So if there are 8MM rows
          you only need 1MB.
        * Then you AND these.
        * If there are too many rows, you can use a page-indexed
          bitmap. But then you need to look at every row in the page.
* GiST
    * Maps ranges to records. Ranges are the key.
    * In a node, ranges are unordered and can overlap.
    * There is no one place where a tuple must belong.
    * Idea is to support GIS, bounding box queries, nearest neighbor.
    * Note this is *generalized*
        * It's just about a property that everyone under that key
          shares.
        * A common use is full-text: map each document to a word
          indicator vector. But you need to do this in a lossy way to
          compress.
        * Then you're searching for this in the GiST.
        * But now you have false-positives, which means unnecessary
          fetches.
        * That leads to slower read performance. They say GIN is 3x
          faster than GiST for full-text.
        * But GiST tends to be faster to update, so that's the
          tradeoff.
        * GiST can be smaller, prolly because don't need to store the
          words in index.
    * TODO: not sure why GiST is really useful for this hashing
      idea. Presumably to find "ranges" of popped indices in the
      bitmap.
* SP-GiST
    * TODO
* BRIN
    * TODO

Sources:

* https://en.wikipedia.org/wiki/Bitmap_index
* https://www.pgcon.org/2016/schedule/attachments/434_Index-internals-PGCon2016.pdf
* http://gist.cs.berkeley.edu/gist1.html
* https://www.postgresql.org/docs/9.1/static/textsearch-indexes.html
* https://github.com/postgres/postgres/tree/master/src/backend/access
    * Each index type has a README which explains how it works!
    * I haven't reviewed this yet, but it seems super useful!
