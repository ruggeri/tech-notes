Say want to represent items `0...Z`.

First approach is to keep a bitset. This uses space proportional to
the cardinality of the universe, though. That sucks.

OTOH, this is optimal if all subsets of the universe are equiprobable.

We can use a list of values, which uses space proportional to number
of items. However that has slow lookup. This uses ~`n * log Z` space.

We can use a hash set, which uses space proportional to number of
items, and has fast operations. This uses ~`2 * n * log Z` space.

Let's do a back-of-the-envelope. Say you want to store values
`0...2**64`. Then there's no way to use a bitset here. So let's say
you use a list of code-points. Well, each item is represented by `64`
bits. Then you can store 125 million entries in 1GB of RAM, or 125K
entries in 1MB of RAM.

Now let's say that you took the low 32 bits of a number, and stored
this in the hash set. Then you could double the number of entries
stored to 250K in 1MB or RAM. Each of these 250K entries misrepresent
`2**32 - 1` items. That sounds like a lot, but remember that there are
`2**64 - 250K` negatives, so this `2**32 - 1 * 250K` is not that
many. It works out to ~0.006%. (Note this calculation overestimates
the error, because it assumes no every item in the set hashed to a
unique value, maximizing the number of collisions in the negative
set).

The problem is that in order to double the number of codepoints we can
store, we have to halve their length. But that's a huge problem,
because it way more than doubles the number of colliding items per
hash.

OTOH, using a bloom filter, and assuming each item takes 1.44
log(1/eps) bits per item, we can store 800k items in 1MB. That would
have an ~40% error rate with a lsit of hashes.

The
chance of a pair of items being a collision would be `2**-32`. There
are many items in the filter, but 

## Bitset

Say you want to store sets of integers `0...Z`. There are `2**Z` such
subsets, so we will need `2**Z` code points. One way to do this is to
identify each subset with a corresponding code point `Z` bits in
length. The obvious way to do this is to use the `i`th bit to
communicate the presence or absence of the value `i`.

This is convenient, because it allows you to query for presence of an
item in `O(1)` time. Not all encodings would have this property.

For a *universe* which is not the integers `0...Z`, then if you have a
way to bijectively map the universe onto `0...Z` we are good. A
*perfect hash function* is an injective mapping onto `0...Z`, it might
be a little wasteful.

The naive encoding seems obvious, since the optimal way to represent
the presence of a single item is a single bit. Since there is no
mutual information between any items, we should just use a bag of the
best encodings of each single item, which leads to this naive
encoding. Therefore, our representation minimizes average code length.

## Trading Space and Accuracy

Let's say that `Z` is large enough that we don't have that many
bits. For instance, it may be common that `Z=2**32`.

In that case, it is necessary for sets to share
representations. You're trying to do a lossy compression.

When we see a representation, we don't know what set that
means. Therefore, if we are querying whether an item is in the set, in
the case that the item is not in both sets, it is unclear, and we must
allow for the possibility of an incorrect result.

If we say that false negatives have infinite cost, and are thus not
allowed, then we must treat each representation as the union of all
sets that map to this code point.

## Set of Fingerprints

One simple approach is to keep using our bitset representation, but
first do a lossy compression of the items: map the `0...Z` into a
smaller space, then represent subsets of this space as a bitset.

Say we set our hash space to half the size of original space. Now each
bit in the bitset represents two items. If the set had only one item
before, then there is now one false positive; if the set had two
items, there are now two false positives, etc.

If we halve again, then then a set with one item will report three
false positives, one with two items will report six false
positives. If we halve again, then it becomes 7, 14, 21...

What we see is that halving the space tends to double the number of
false positives.

We can also see here that the number of false positives tends to be
proportional to the number of items in the set.
