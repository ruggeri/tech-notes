## MinHash

You want to quickly assess the similarity of two documents. For
instance, you are Altavista and you want to dedup webpages. You might
want to use Jacard similarity: the ratio of the intersection and union
of words in the documents.

So, hash all the words in each document; take the minimum. The
probability that the min hash is the same in both documents is equal
to their Jacard similarity.

This is an unbiased estimator, but with very high variance. To get
greater resolution, you can hash with multiple hashing functions, or
with different salts. Alternatively, to avoid too much hashing, you
can just pick the smallest `k` elements wrt a single hash.

## Random Projection

Let's step away from documents momentarily. Say you have vectors in
space, and you want to find those vectors with the greatest cosine
similarity.

In that case, generate a bunch of hyperplanes. The probability that
the two vectors lie on opposite sides of the hyperplane depends on the
angle between them.

The hyperplanes translate each vector into a bitstring. The hamming
distance of the bitstrings (number of bits different) reflects the
cosine distance between the two.

This is extremely similar to MinHash for documents; it's just a
version for vectors.

There even appears to be a way to do this for *Euclidean distance* of
points. You choose random lines, project points onto the line, and
then see whether the points are within a certain distance on the
projected line. I didn't study this in depth, but I make a note here
in case for the future.

## Locality Sensitive Hashing

Okay, so given these random projections, how can we quickly find
similar matches?

I believe the idea is to keep a hash map for each dimension. In the
case of MinHash, you do a lookup for each hash: you find every
document that agrees with the MinHash for any dimension. You calculate
the similarity of these directly.

To reduce the false positive rate, I assume you can use a hash map
where the key is the result of two MinHashes. That seems like it would
be really necessary for the bitstrings produced for random
projections.

Anyway, this should get you nearby stuff.

**TODO**: Sometimes this is used for hierarchical clustering? How?

## Feature Hashing

Say you have a big, sparse vector of features. A good example is a
text document as a bag of words; a one or a zero for each word. This
is a lot of not-very-dense information.

So what you can do is take each present feature `f`, hash it to `h`,
and increment `vec[h % N] += 1`. This is a dense projection.

TODO: Gaurav once claimed that this is not just an optimization, but
can yield *better results*. Peter and I didn't understand this. I
still don't.
