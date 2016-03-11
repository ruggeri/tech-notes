* Closed hashing with linear reprobing improves cache friendliness.
    * A lot better than scanning LL.
* Avoid allocations in get/put.
* Memoize the hash when stored. Then, on retrieval, compare keys to
  see if they point to the same object, hashes to see if they have the
  same hash, and only if the hashes compare do a deep equality.
    * Otherwise you're always doing a deep equals which is slow.
* Instead of doing MOD, use a power of 2 number buckets and do an
  AND. That helps because MOD is 30x slower than AND.
