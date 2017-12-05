## MinHash

You want to quickly assess the similarity of two documents. For
instance, you are Altavista and you want to dedup webpages. You might
want to use Jaccard similarity: the ratio of the intersection and union
of words in the documents.

So, hash all the words in each document; take the minimum. The
probability that the min hash is the same in both documents is equal
to their Jaccard similarity.

This is an unbiased estimator, but with very high variance. To get
greater resolution, you can hash with multiple hashing functions, or
with different salts. Alternatively, to avoid too much hashing, you
can just pick the smallest `k` elements wrt a single hash.

**Pedantic note**: There are some subtleties. Technically it's hard to
randomly sample permutations of the `n` elements in the vocabulary
(which is what you're really doing). And using the `k` smallest hashes
deviates a little from the optimal of using `k` random hashes...

## Random Projection

Let's step away from documents momentarily. Say you have vectors in
space, and you want to find those vectors with the greatest cosine
similarity.

In that case, generate a bunch of hyperplanes. The probability that
the two vectors lie on opposite sides of the hyperplane depends on the
angle between them.

The hyperplanes translate each vector into a bitstring. The hamming
distance of the bitstrings (number of bits different) indicates the
cosine distance between the two.

This is extremely similar to MinHash for documents; it's just a
version for vectors.

There even appears to be a way to do this for *Euclidean distance* of
points. You choose random lines, project points onto the line, and
then see whether the points are within a certain distance on the
projected line. I didn't study this in depth, but I make a note here
in case for the future.

## Locality Sensitive Hashing (LSH)

Okay, so given these random projections, how can we quickly find
similar matches?

You just use many locality sensitive hashes. You use these to map to
buckets. You iterate through buckets, and hopefully only look at a few
items. You calculate the full similarity (or something close) with the
small number of items in those buckets.

## Feature Hashing

Simple feature hashing just hashes the features and does a
dimensionality reduction. This has an advantage of no storage cost for
dictionaries. Also helpful when you could encounter new features
online and want to do online updates to weights.

Gaurav once claimed that feature hashing can *improve* performance and
Peter couldn't believe that. I think the idea is that dimensionality
reduction this way can serve as a form of regularization or
prior. Imagine you're doing logistic regression, and then are a bunch
of unique features. Then they can steal all the weight and now you
won't use the other features.

When you feature hash, you will put a certain number of useless
features in with these low-reach features. I feel like maybe this
implicitly performs a Beta prior by throwing in something like
pseudocounts.
