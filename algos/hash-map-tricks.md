* Closed hashing with linear reprobing improves cache friendliness.
    * A lot better than scanning LL.
* Avoid allocations in get/put.
* Memoize the hash when stored. Then, on retrieval, compare keys to
  see if they point to the same object, hashes to see if they have the
  same hash, and only if the hashes compare do a deep equality.
    * Otherwise you're always doing a deep equals which is slow.
* Instead of doing MOD, use a power of 2 number buckets and do an
  AND. That helps because MOD is 30x slower than AND.
* You might want to use arrays instead of LL if you do closed
  addressing, as this involves less pointer chasing.
    * The overhead of resizing is expected `O(1)`.
* Cuckoo hashing allows for `O(1)` guaranteed lookup, at the expense
  of possibly more work when inserting.
    * But you won't have the cache performance of linear reprobing.
    * NB: linear reprobing is helpful when records are small, since
      more items can fit in a cacheline, meaning locality can help
      more.
    * Sounds like cuckoo hashing only helps in some specialized
      applications (for instance, if entire table is resident in
      cache).
    * Prolly due to jumping around...
* Incremental resize:
    * You can do this by keeping two copies of the hash map and
      performing resize over time.
    * But it doesn't help you avoid rehashing.
* Linear hashing:
    * Summarized in the databases book. Really for on-disk hash maps.
    * Always have hashes modulo a power of large power of two (like
      2**64).
        * If there are `i` buckets, use the last `log(i)` bits to
          place the item.
        * If the hash is greater than the current number of buckets,
          drop the top bit.
    * "Add" one bucket per insertion.
    * Notice that you will need to allocate more blocks; you're not
      going to be copying anything over.
        * Though you could still have copying, just no rehashing...
        * This means indexing means looking up blocks, which could be
          `O(#blocks)` if stored in a LL. Or could have a dynarray of
          blocks.
        * Whatever, the most important thing is to avoid reading from
          disk!
    * When you add a bucket, you might have to split up a bucket. But
      that's O(1) work.

## Perfect Hashing

* Ideal hash function takes in items, and in constant time produces a
  hash, which is unique for that item.
* If you have a set of size `|S|`, you could theoretically do this
  with `log(|S|)` bits.
    * But can you really compute such a hash in constant time?
* TODO: I don't actually know how to make a perfect hash.
* I think the idea is that you're going to have an array of size `|S|`
  and use perfect hashing to map `S` to indices, allowing all WC
  `O(1)` operations.
* Cuckoo hashing would get you most of the way there, except insert
  could be unbounded.
* Also wouldn't be s cache friendly.
* Whatever, I don't think I care!

## TODO

* Finish perfect hashing.
* Consistent hashing (minimize rehashing work)
* Extendible hashing
