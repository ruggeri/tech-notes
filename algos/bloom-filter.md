A Bloom filter is used to prevent searching for misses in a more
expensive datastructure. It is very effective when most of the queries
are for keys that don't exist.

A Bloom filter can give false positives, but never a false negative.

Bloom filter is a bit array. You use `m` bits, and `k` hash
functions. When you add something, you hash `k` times, setting each of
the bits. To check if something is there, you just check if all `k`
bits are set.

One big advantage to the bloom filter is that you don't store any
values, which can be fairly large. You don't even store the *hash* of
the values.

The one thing that can be confusing is the use of multiple hash
functions. Think if only one hash were used; then, in order to keep a
low collision rate, you would need to have very many zeros in the bit
array. That would be quite wasteful.

In a sense, by using multiple hashes, you are "combining" or
"compressing" the distributed representation of an element.

You can use stats to calculate the false positive rate for a given
number of items (`n`) and bits (`m`) and hashes (`k`). Then you can
calculate the best number of hashes for `n` and `m`. Typically you
make `m` as large as you can. Alternatively, you can start from a
false positive rate and pick `m` smallest such that the proper `k`
will achieve that rate.

You can do union easily with Bloom filters. You can also do
intersection, with some error (since the same bit can be set in both
filters as a result of inserting different elements).

A good example of a use case is Akamai: nothing is cached until
requested twice (to prevent "one-hit wonders"). Previous requests are
placed in a Bloom filter. Databases also use this typically.
