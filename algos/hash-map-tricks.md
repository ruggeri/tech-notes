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

## TODO

- Consistent hashing (minimize rehashing work)
- Extendible hashing
