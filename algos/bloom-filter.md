## Set of Hashes

Let's say you were just storing hashes of items added to the
filter. Then if your hash is of `m` bits, the probability of two items
colliding should be `.5**m`; the probability of not colliding is thus
`1-.5**m`. Consider any item you are testing that *isn't* in the set;
the probability of all `n` items in the set not colliding with your
test item is `(1-.5**m)**n`. Thus the probability of getting a false
positive is `1 - (1-.5**m)**n`. I can also express this as `1 -
(1-2**-m)**n`.

How big should `m` be? It depends on `n`, and the desired false
positive rate, `eps`. Setting the false positive rate to `eps`, we get:

    m = -log_2 1 - (1-\eps)**(1/n)

I have trouble analyzing this. Maybe I can think the other way
round. Say there are `m` bits; that's `2**m` buckets to place items
in. Then, if I add another bit, I'm doubling the number of buckets. In
that case, I can double `n` but maintain the same average size per
bucket. The average size per bucket tracks the false positive rate;
technically we shouldn't weight by size of bucket, but presence of
*any* item in the bucket, but if the number of buckets is large, than
it is that much less likely for a bucket to have two items!

Therefore, I see that the number of bits needed should grow
logarithmically in `n`. This shows that the storage needed, overall,
is `O(n log n)`.

Likewise, I see that to halve the false positive rate, it is necessary
to double the number of buckets, thereby necessitating an extra
bit. So we should have `m` in `O(-log_2 \eps)`.

But here's a thought: why are we storing `n` of these fingerprints of
length `O(log n)`? We can have an equivalent representation by using
`2**m` bits, and just popping them when that hash is added. This is
entirely equivalent. This uses `O(1)` space per item.

So now we have gotten to a bloom filter with a single hash
function. We see that, once you've set the desired false positive
rate, you only need space proportional to the maximum required size of
the bloom filter to acheive that rate.

NOTE: I think this shows that you can have a linear space cost for a
bloom filter even with simply `k=1`. A bloom filter is really no more
than a space-efficient hash set of fingerprints (as described above),
with an optimization where `k` is chosen appropriately.

Further note: we can use `O(n)` memory to store all the items. This
has slow membership querying (`O(n)`). We can acheive `O(1)` lookup by
using perfect hashing and using a bitset. Note that a perfect hash is
just an identification of elements to integers.

The point of this probabilistic structure is that we don't have a
perfect hashing.

Let's consider `k`. Say that you double `k`: this increases the
percentage of buckets with an element by about two. On the other hand,
you need twice as many positive hash matches. If `k` gets too high,
then it's clear that you'll exceed an average count of one in each
bucket, so that it doesn't matter that you look at multiple
buckets.

Does doubling ever help? Well, let's consider! You want

    n/m > ((2n)/m)**2

When does that happen? Well, dividing both sides this is the same as
saying:

    1 > 4 * (n/m)

This is basically saying, that you if you can double the number of
hashes, but still be less than 50% full, you want to do that. Of
course there are constants involved, but this suggests that to halve
the false positive rate, you double the number of buckets (thus
space), but then you probably want to increase the number of hash
functions by one.

NOTE: Everything here sounds a lot like lossy compression, and the
theory around that. It sounds like bloom filters et cetera are just
trying to get closer to some kind of optimal.

## Bloom Filter

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

## Cuckoo Filter

Conceptually: add a fingerprint of the key into a hash set. This will
allow probabilistic removal, btw. Chaining is done via cuckoo hashing,
so lookup is O(1). One advantage is that you will only look in at most
2 locations for the fingerprint, whereas you need to look in `k`
locations for a bloom filter.

Sources:

* https://www.cs.cmu.edu/~dga/papers/cuckoo-conext2014.pdf
