## Basics

* A dense index stores keys and pointers in sorted order; this should
  be much smaller.
    * We can do binary search in the index. It may fit in main memory.
* Instead, we can use a sparse index. This doesn't have entries for
  every key, but for a subset. Still can get us quickly to the right
  block. Can have multiple levels of index, if the sparse index still
  isn't small enough.
* Indexes can be primary or secondary. Secondary indexes must be dense
  (since records are not arranged via non-primary key), though we can
  layer sparse indexes on top.
* When we're storing records in a heap (vs sequential files), we'll
  want this "secondary" index.
* An *inverted index* is relevant for document retrieval.
    * For every word in the document, you put an entry in the
      index. You only put it if the word is present (it's true/false,
      so absence is clear).
    * A common optimization is to use *buckets*, each word in the
      index points to a bucket, which contains the id of every
      document with that word.
    * To query based on several words, you intersect the buckets.
    * Note: the words may be keywords; this is the opposite of a
      "forward" search engine index.
    * You can also store position in the document, which may enable
      more powerful queries like phrase queries.

## B-Trees

* They talk about B+ trees specifically.
* The leaf nodes hold the "values", which are keys and offsets to
  where the key is located.
* They note that if the number of keys per block is high, splitting
  and merging is rare.

## Hash Tables

* You have buckets, they store the records directly inside. Then you
  have an index to the blocks.
* Obviously no range query optimization. But possibly only one disk
  seek to find the record you want.
* But eventually you'd have to grow.
* One way is to do a normal extension, but not break up the bucket
  blocks just yet. You lazily split up buckets; two indexes in the
  bucket table can refer to the same bucket. But when this becomes
  full, you do really need to break it up.
* One defect of this strategy is that you don't just want the average
  size to be small; you want the *worst* size to fit in a block. So
  you might have to do a lot of unnecessary resizing just to break up
  a bucket that's too big.
* Linear hashing allows overflow, but it is going to increase the size
  more slowly. The problem is, that since it's not doubling in size,
  contents of buckets get more scrambled (not just broken into two
  buckets). So we have a potential problem.
    * What we do is we grow one bucket at a time, as needed.
    * So there are `n` buckets, and `i` bits needed to specify the
      largest numbered bucket.
    * When placing an item, use the last `i` bits. But if this is
      greater than `n`, drop the high bit to zero (use `i-1` bits).
    * When adding a bucket, you split the corresponding bucket where
      you would have dropped the leading bit.
    * Note you'll need continuation blocks for overful buckets.
    * But importantly: you don't have to rehash everyone at once!

## Multidimensional Indexes

* Points and regions defined in the space.
    * You may want to find matches in a single dimension.
    * You might want ranges, finding points in the set, or objects
      that overlap (or are contained).
    * Nearest neighbor queries.
* You could try to use multiple conventional indexes.
    * One problem is if each dimension gives us little discriminative
      power. Then each index returns a bunch of results.
    * To answer nearest neighbor, you can slowly expand out from a
      given point, giving larger and larger ranges.
* You can try to make a grid file, which is a multidimensional array,
  where each element points to a region (parallelpiped) in the array.
    * This can be fast, but I'm not sure how I'd grow this.
    * Also, because you're dividing each dimension into ranges, you
      might generate a lot of useless regions. This wastes space.
* We could also try to use a "partitioned hash function". This
  allocates some bits of the hash for each dimension. That would allow
  searching with partial data; the number of buckets you need to look
  at is proportional to the number of missing entries.
    * However, still no ranges. Just good for missing dimension.
* Multiple key indexes.
    * You have an index on one attribute, which leads to an index on
      the next.
    * This is fast for partial match (if you have the 1st component).
    * Also good for range queries. And that gives you nearest
      neighbors without too much trouble.
* kd-Trees
    * These are kind of like BST, except each node can partition by
      any attribute. With two dimensions, these would alternate.
    * The trouble is keeping this balanced. One way is to split along
      any chosen dimension when a block gets filled. This shouldn't be
      *too* bad with random points.
    * But this greedy approach surely cannot be very ideal.
    * Missing data search kinda sucks.
    * Range queries can be better than multiple-key, I expect, since
      you don't need to search once per coordinate in one dimension.
    * To adopt to secondary storage:
        * You could try to have higher branching, but that's not
          trivial.
        * You could try to store interior nodes together in the same
          block.
* Quad-Trees
    * Kinda like kd-trees, except you always partition into four
      squares each time.
    * Oct-trees just do this for three dimensional data.
    * You could try to split more ways, but I think the minimum is
      best. That way you avoid wasted regions (in case some are
      unpopulated), and you can always "pack" your blocks to contain
      multiple buckets.
* R-Tree
    * This is used to store regions.
    * Each node has a region.
    * When you insert, you go down into the region that contains you.
    * We may have to "expand" regions, if the region doesnt really fit
      anywhere.
* Bitmap Index
    * For each key, you store a bit per record, to say whether it is
      present.
    * This seems wasteful, but it depends on the number of hits. We
      may be able to significantly compress if mostly zeros.
