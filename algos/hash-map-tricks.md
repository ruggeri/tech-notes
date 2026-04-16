# Closed vs Open Hashing

- "Open Hashing" is also called "separate chaining."
  - Basically, you're going to use buckets, typically a dynamic array or
    LL.
  - LL will not trigger bucket resizing, but array will involve less
    pointer chasing.
  - This is typically the simplest implementation.
  - Easy to delete: just remove from a bucket.
  - Handles load all the way to 100%
  - Has more pointer/object overhead. Cache locality is hurt by
    following pointers.
- "Closed hashing" is also called "open addressing."
  - Here, you always store keys/values in the hash map.
  - This reduces pointer overhead, is more compact, can have better
    locality.
  - Question becomes: how do you handle hash collisions?
- Linear probing
  - An open hashing technique where you start at a hash index, and you
    just keep stepping forward until you find a free cell.
  - To store, you store at the first free cell.
  - To lookup, you may have to keep examining buckets until you (1) find
    the query key in the table, or (2) find an empty cell.
  - As table fills up, performance will degrade.
  - Deletion can be slow; you need to scan forward for keys that can now
    be placed in this bucket.
    - There's also a lazy deletion idea where you mark the key as
      deleted and only do cleanup later?
  - Can give good performance when load factor can be kept low, and
    locality of reference will provide a big win.
  - Choice of hash function matters more than usual, because not just
    hash collisions, but also _clusters of nearby hashes_ will degrade
    performance.
- Double Hashing
  - An alternative to linear probing.
  - The idea is to avoid the clustering problem.
  - You use a _second_ hash of the key. This becomes a _stride_. You
    stride from the initial position, looking for the key (or the first
    empty cell).
  - This gives poor locality of reference, but basically eliminates the
    clustering problem.
- Quadratic Probing
  - Another alternative to linear probing.
  - Basically, examine offsets `H + 1**2, H + 2**2, H + 3**2...`.
  - This reduces clustering problem, but still preserves some locality
    of reference benefit.
- Cuckoo Hashing
  - Easiest explanation is: two tables, two hash functions. Each key has
    two possible homes.
  - Lookup of a key will look at `T_1[h_1(x)]` and `T_2[h_2(x)]`. This
    is `O(1)`.
  - On insert, may need to move keys around. You insert at
    `T_1[h_1(x)]`. But if a key already lives there, move it to its
    position in the second table. If someone lives there, move it to the
    first table...
  - Insert performance degrades with load factor.
  - You can have cycles! Rather than cycle-detect, you'll want to set a
    `MAX_TRIES`. In that case, you'll need to reallocate tables and
    re-choose hash functions.
  - Can also do this in one table with two hash functions.
    - When moving a key, you might have to calculate both hash values
      because you don't know in advance which will give you the new
      position.
    - You might have `h_1(x) = h_2(x)`. But you can fix that with a
      deterministic rule.
- Note: the problem of "primary clustering" we've been exploring is
  _not_ just about a poor hash function.
  - It is a statistical phenomenon of open addressing.
  - Basically, "runs" of consecutive filled positions will tend to
    increase in length. And eventually, they will start to _join_.
  - Even a hash function which is uniform over the buckets will display
    this phenomenon.

# Performance Tricks

- When storing a key, also store its hash.
  - Then, on retrieval, you can compare hashes first (without rehashing
    the stored object).
  - Only if the hashes match do you do a key comparison.
  - This saves time if key comparison is deep/expensive.
- Instead of doing MOD, use a power of 2 number buckets and do an AND.
  That helps because MOD is 30x slower than AND.

## TODO

- Incremental resize:
  - You can do this by keeping two copies of the hash map and
    performing resize over time.
  - But it doesn't help you avoid rehashing.
- Linear hashing:
  - Summarized in the databases book. Really for on-disk hash maps.
  - Always have hashes modulo a power of large power of two (like
    2\*\*64).
    - If there are `i` buckets, use the last `log(i)` bits to
      place the item.
    - If the hash is greater than the current number of buckets,
      drop the top bit.
  - "Add" one bucket per insertion.
  - Notice that you will need to allocate more blocks; you're not
    going to be copying anything over.
    - Though you could still have copying, just no rehashing...
    - This means indexing means looking up blocks, which could be
      `O(#blocks)` if stored in a LL. Or could have a dynarray of
      blocks.
    - Whatever, the most important thing is to avoid reading from
      disk!
  - When you add a bucket, you might have to split up a bucket. But
    that's O(1) work.

## Perfect Hashing

- Ideal hash function takes in items, and in constant time produces a
  hash, which is unique for that item.
- If you have a set of size `|S|`, you could theoretically do this
  with `log(|S|)` bits.
  - But can you really compute such a hash in constant time?
- TODO: I don't actually know how to make a perfect hash.
- I think the idea is that you're going to have an array of size `|S|`
  and use perfect hashing to map `S` to indices, allowing all WC
  `O(1)` operations.
- Cuckoo hashing would get you most of the way there, except insert
  could be unbounded.
- Also wouldn't be s cache friendly.
- Whatever, I don't think I care!

## TODO

- Finish perfect hashing.
- Consistent hashing (minimize rehashing work)
- Extendible hashing
- Universal Hashing
