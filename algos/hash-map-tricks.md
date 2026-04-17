This is really more like a lot of ideas in hashing and hash maps, rather
than a collection of "tricks".

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
- Incremental resize/rehashing:
  - This is the typical deamortization idea. With a dynamic array, it
    involves allocating a 2nd buffer at 50% full, and copying over two
    items for every one you insert.
  - You can do the same thing with a hash map.
  - Approximately triples memory use. But can make sense if you care
    more about latency than memory use.
  - Will work easier with chaining rather than open addressing (where
    items can maybe move around on you).

# Perfect Hashing

- If you have a set of size `|S|`, you ideally want to map each key `s`
  to a unique value in `0...|S|`. Each key needs `log_(|S|)` bits.
  - This is called a _minimal_ perfect hash. There are no collisions.
  - If you map keys `s` to a value `0...N` with `h(s1) != h(s2)`
    whenever `s1 != s2`, that is a perfect hash. If `N > |S|`, then it's
    not minimal.
- To construct a perfect hash, you generally must know the size of the
  set that you are hashing in advance.
- If the keys are integers `0...N`, then the identity function is
  already a minimal perfect hash.
  - But if the keys are integers `{100, 345, 999}` or something, then
    identity function is a _perfect hash_, but not a _minimal_ perfect
    hash.
- Consider strings of four 8bit chars. You can interpret this as a 32bit
  integer. The mapping from four-char string to 32bit integer is a
  minimal perfect hash for the set of four char strings.
- The point of perfect hashing is when you have _irregular_ sets.
  - For instance, your key set will be 50 strings, each consisting of 4
    8bit chars.
  - Yes, the mapping of the string to a 32bit integer is still a perfect
    hashing.
  - But it is no longer _minimal_. If you achieve minimality, you can
    create a hash map for these 50 strings using exactly 50 buckets,
    with open addressing, O(1) lookup and insert.
- An example would be mapping enum names to values.
  - Yes, the compiler will just translate the names to values for you.
  - But how does the compiler lookup those values at compile time? It
    could just iterate the list of enum names.
  - But that is proportional to the length of the enum set.
  - It's fine for a compiler, which will only compile the program once.
    But we can imagine programs which dynamically generate new enums and
    must lookup enum values by name on-the-fly.
- There are some fancy algorithms that can:
  - Build a perfect hash function in `O(n)` time.
  - The hash function requires `O(n)` storage of "hints" to help
    calculate the hash.
  - The hash function takes `O(1)` time to consult the hints and compute
    the perfect hash.
  - I'm not going to actually explore constructing a perfect hash, I
    just wanted to define what perfect hashing _is_.
- Perfect hashing can be useful when you have static keyword tables or
  dictionaries. It isn't an entirely theoretical idea, but it isn't
  general-purpose either.
  - There are even dynamic versions that will allow keys to be added?
    See Dietzfelbinger. Not sure if this is even more niche...

# Universal Hashing

- Say you want to analyze hash map performance. You want to find
  _expected_ run time.
  - But expected with respect to what? Are you fixing a hash function,
    and then assuming random key selection? Maybe uniform key selection?
  - But what if key selection is _not_ random? What if it is possibly
    even _adversarial_?
- Instead, you can randomize with respect to choice of _hash function_.
  - A _family_ `H` of hash functions is called universal if, for every
    pair of keys `x, y`, the probability that `h` sampled from `H` gives
    `h(x) = h(y)` is less than `1/m` (where `m` is the number of
    buckets).
- You can prove chaining hash maps have `O(1)` expected time so long as
  the hash function meets this definition of universality.
- However, to prove `O(1)` bounds for linear probe open addressing, you
  do need a stronger property...
- Universal Hashing For Integer
  - To hash an integer, `h_{a, b} = ((ax + b) mod p) mod m`.
  - `p` must be a prime greater than the largest value to hash. `m` is
    the number of buckets (must be less than `p`).
  - `a, b` are chosen randomly.
  - Note: I don't prove here this actually is a universal hash function.
    I'm just trying to give an example.
  - If we are hashing 32-bit words, then `MAX_VALUE = 2**32 - 1`. And
    we're supposed to have `p > MAX_VALUE`.
  - You could definitely put `p` in a 64-bit word. Then, you promote
    your 32-bit word to hash to a 64-bit word, and do all math in 64
    bits.
- Practical Universal Hashing
  - To hash a 32-bit number, you can instead choose uniformly `uint64_t
a`.
  - To calculate an `r`-bit hash you do: `uint32_t ((ax) >> (64 - r))`.
  - I believe this does not meet the strict universal hashing property.
    But it has a weaker property which I think is good enough for hash
    map bounds.
- You can extend to a hash of a string (an array of bytes). This is
  something like:

```C
uint hash(String x, int a, int p)
	uint h = INITIAL_VALUE
	for (uint i = 0; i < x.length; ++i)
		h = ((h*a) + x[i]) mod p
	return h
```

- Like I said, I won't prove that these actually have the universal hash
  property. That could be worthwhile, but it's not my goal. I just want
  to describe what universal hashing is.
- Universal hashing has an "oblivious adversary".
  - That is, the adversary doesn't get to see the hash values. They can
    know the hash family, but not its parameters.
  - They might be able to easily compute those parameters if they saw
    the values of the hash function for selected keys.
  - Even if they just see some hash values, but neither get to select
    the keys, nor even _see the keys_, they might recover the
    parameters.
  - That suggests that these constructions aren't really secure to a lot
    of attacker models.
    - Consider even an attacker who can observe how long key
      lookup/insert takes. That could be a form of timing attack which
      could leak information about parameters.
  - They really are more about proving bounds on hash set performance.

# Extensible Hash Table

- Also called "extendible hashing". My notes summarize the Garcia-Molina
  et al Databases book (p652). The context is on-disk block storage.
- All keys will get hashed to `k` bits, where `k` is large.
- There will be a block index, mapping `i < k` high-order bits from the
  hash to blocks. The block index will always have size `2 ** i`.
- The block index values are handles to disk blocks. Blocks have a fixed
  size: `BLOCK_SIZE`. We will load the block from disk to RAM, and then
  linearly search. The values stored in the blocks are unsorted.
  - This avoids overhead for keeping things sorted inside a block.
  - Disk read time dominates, so optimizing for search within a loaded
    block is not important.
- Here is our goal: when a block fills up, we want to split it, but we
  (1) don't want to always resize the whole block index, (2) we don't
  want to rehash items in other blocks.
- Each block is going to keep a "nub" value `j`.
  - When we first build the block index and allocate the blocks, we will
    set `j = i`.
  - Keys placed in the block will always match in the last `j` bits.
  - When a block reaches capacity, we will split it. The new blocks have
    `j' = j + 1`. We divide keys between the two new blocks based on the
    last `j` keys.
- When `j > i`, we will also double the size of the block index.
  - Say `i = 4` and it is time to double block index size to `i = 5`.
  - Then bucket at `0101` will be stored at _both_ `00101` and `10101`.
  - This bucket will still have `j <= 4`.
- Pros/Cons
  - Yes, resizing the block array still stops-the-world. But it is
    simply doubling the size of an in-memory datastructure. Most of it
    is just copying.
  - You avoid reading all the blocks when one bucket needs to be split.
    Pulling those from disk (and writing them back out after splitting)
    would be terribly painful.
  - You could use this technique for in-memory hash maps with chaining.
    But you would not get a great deal of benefit. You have `num_buckets
~ num_elements`, so the work of allocating a new buckets array is
    already approximately equal to the work of just creating new
    buckets and copying elements to their proper place.
  - The real payoff comes when you have big blocks, and time to fetch
    them dominates.

# Linear Hash Table

- Summarized in the databases book (p655). Again, for on-disk hash maps.
- We are going to have still use hashes of `k` bits.
- There will be `n` buckets, but we will allow `n` not to be a power of
  two. We will let `i = ceil(log_2(n))`. `i` is the number of bits to
  specify one of `n` buckets.
- To query/place a key, calculate its hash, and mask the low `i` bits.
  If the hash `h < n`, query/place the key at bucket `h`.
  - If `h >= n`, then set `h <- h - 2**(i-1)`, and query there.
- When inserting, we may run out of space in a disk block. When that
  happens, we chain a second block.
- Now, we are also tracking the load factor: `num_items / n`. Whenever
  this exceeds a threshold after an insertion, we will "add" a bucket.
  - We are adding bucket `n`, the `n+1` bucket. We know that the `i`-th
    bit of `n` is necessarily 1.
  - Therefore, we must split the bucket that just replaces the `i`-th
    bit of `n` with a zero.
- When adding a new bucket would roll over `n` to a power of two, and
  increase `i` by one, you must reallocate the bucket array.
  - This is still fairly cheap. You only need to copy the `n` buckets,
    you don't even need to set the unused buckets yet.
- Now the overflow of a single bucket does not trigger a resize of the
  entire block array.
  - That is helpful, because extensible hash table block array can run
    away in size if we are unlucky and keep splitting one unlucky block.
  - With linear hash table, in _expectation_, we still only look at one
    block when querying. In expectation, we only have to read one block.
- With both extensible and linear hash tables, it feels like you should
  allocate the block index as big as you can up front.
  - That way you _never_ need to allocate a new array. Plenty of space
    for linear hash table buckets to grow into. And entire extensible
    hash table bucket array is set from the start.
  - But you only create a few blocks up front. You can split along the
    way.
  - Could make sense for a database machine managing a single table. On
    the other hand, this isn't the only table and index on the system,
    so you may want it to only take space proportional to the number of
    elements stored.
- Even if you start with a short buckets array for extensible hash map,
  it is not costly to grow.
  - You copy `O(n / BLOCK_SIZE)` bucket pointers. That's all in memory;
    it's not expensive.
  - Yes, "unlucky" buckets that get overfull can trigger early splits.
    But when `BLOCK_SIZE` is large, that is very unlikely, because of
    law of large numbers.
- So when `BLOCK_SIZE` is large, it feels like extensible hash table
  doesn't have a real problem...
  - So maybe linear hash table is better when `BLOCK_SIZE` is small?
  - Maybe, but that would only make sense for an in-memory map.
  - In that case, I suppose linear hash table might make sense as a
    deamortized of a chained hash table?
  - With that said, you must re-initialize the buckets array with the
    existing buckets (that's `O(n)`).
  - Why not create all `2n` new buckets? It's only double the work. And
    then, the items are already resident in memory, is it really so
    costly to put them in the new buckets?
- In conclusion: I doubt that the linear hash table idea is some
  phenomenal win ever.
  - Argued with ChatGPT a lot. It seems to insist that linear hash table
    can have value.
  - Specifically suggests that if the bucket ID is itself a _logical
    page ID_, which will be allocated sequentially (and not
    deallocated?), and managed regardless by a storage management
    subsystem.
  - In that case, you can avoid adding a layer of indirection where you
    must map `key -> hash -> bucket id -> logical page ID -> physical disk location`.
  - May also be a proto-idea for distributed system consistent hashing.

# Consistent Hashing

- Consistent hashing is a goal or property, not one technique.
- The context is distributed systems. We will have many peers hosting a
  distributed hash table.
  - We will assume that the dominant cost is _communication_.
  - Nodes may be able to hold their entire shard in memory. They are
    assumed to have fast random reads and even fast scans.
  - We plan to scale up the number of nodes if shards stop fitting in
    memory.
- Here is "ring consistent hashing":
  - Each node randomly selects a starting hash value.
  - This divides the hash space into _ranges_; a node manages key hashes
    between its own start value, and the smallest succeeding start value
    amongst the peer set.
  - The ranges wrap back around, to make this a circle of hash values.
- Presuming a client has an up-to-date routing table of node IPs to
  start value, it can easily direct a query to exactly the right node.
- We need to handle nodes entering and exiting the peer group.
- To enter, a new node randomly samples a starting value. It then
  looks up the peer with the largest prior start value.
  - The new peer requests approximately half the data from that shard.
  - Effectively the shard is split, so the work is `O(SHARD_SIZE) =
O(NUM_KEYS / NUM_NODES)`.
  - When a hash function allows resizes to be done with `O(NUM_KEYS /
NUM_BUCKETS)` work, that is what we call a _consistent hash_.
- The peers will need to inform each other of routing updates. They can
  "gossip" about this.
  - I won't describe how routing information is propagated, because it
    isn't really central to the idea of consistent hashing.
- How do we handle a node exiting?
  - Peers will replicate shards. One idea is that each node will store
    the keys for the next three ranges. Each range is stored 3x.
  - Peers track liveness (maybe with pings). When a peer exits, a prior
    node will have to make a new replication of the shard.
  - That again takes `O(SHARD_SIZE)`.
  - Again, these are not supposed to be distributed system notes, so I
    won't focus on entry/exit details...
- I _will_ note that, with dynamic entry/exit, it is not always clear
  who the true "owner" of a shard is. Writes may be accepted by any peer
  that replicates the shard.
  - That can allow for temporary inconsistency. But if replicas always
    eventually share a write, the system should become "eventually
    consistent."
  - Again, a distributed systems problem, not a hashing problem.
- Both the Chord DHT and Amazon Dynamo use this kind of idea.
- There is a related idea: _rendezvous hashing_, more usefully called
  _highest random weight_.
  - Basic idea is, instead of handling ranges, a key will be placed on
    the node `node` for which `h(key, node)` is greatest.
  - By taking the top `REPLICATION_FACTOR` values, we can do
    replication.
  - When a node leaves, we will have to recalculate the top hashes for
    every key in the shard. So the shard gets "broken up" across _all_
    the nodes rather than "merged" into a prior node.
  - This is the same aggregate communication, but involves more nodes.
    It also involves more rehashing work.
  - It is a little better balanced. With ring hashing, you often create
    many "virtual" nodes to ensure a good distribution of keys.
