# Locality Sensitive Hashes

Basic idea is that you have a notion of similarity measure, and you want
a "family" of hash functions such that: `P[h(x) = h(y)] = f(s(x, y))`.

Here, `x, y` are vectors. `s` is a measure of "similarity" (more later).
`h` is a hash function selected randomly from the family of hash
functions `H`. That's where the probability comes from. Last, `f` is a
_monotone_ function, mapping similarity to probability.

Basically: you want the probability that the hashes are equal to be
monotone in the similarity of the vectors. If the similarity itself can
be naturally interpreted as a probability, maybe even `f` is the
identity.

For each kind of similarity, we will typically need a different kind of
locality sensitive hash.

**Metric vs distance vs similarity vs norm vs measure**

- This is kind of an aside.
- Metric is the one with the precise definition, triangle inequality.
- Distance is a less precise word than metric. Sometimes it is used to
  mean the same thing. Other times, it violates one of the conditions of
  a metric, but still measures "dissimilarity."
- `1 - cos(theta)`, where `theta` is the angle between two vectors, is a
  good example of a "distance" which is not a metric. KL divergence is
  another example.
- "Similarity" is similarly imprecise. `cos(theta)` is a classic
  example. Common requirements are (1) symmetric, (2) bounded between
  zero and one, (3) self-similarity is one. But note that even
  `cos(theta)` isn't bounded between zero and one; I guess you could
  square it...
- A similarity or dissimilarity/distance can have a geometric or
  probabilistic interpretation. But there is really no general
  structure to these terms.
- Norm is a measure of length of a vector. A norm naturally induces a
  metric (distance between two vectors is norm of their difference). But
  some metrics are not "norm-induced"; they have no corresponding norm.
- Measure is about size of sets. Length, area, volume, and probability
  come from measure.

# Jacard Similarity/MinHash

**Jacard similarity**

- Typically used for documents. Also for segmentation.
- It is the size of the intersection of A and B divided by the size of
  the union of A and B.
- It's the probability that a word in the union of the two documents
  appears in both of them.
- Common use is to identify duplicate documents. Historically, I believe
  Altavista used this to identify duplicate websites.
- Naive calculation will require loading at least one document fully
  into memory, and then streaming through the other, looking up each
  word in the memory mapped document.
- You could speed up this calculation (at the cost of some noise) by
  using a bloom filter.
- Particularly annoying if you want to find similar documents in an
  index. For one new document, to find the most similar document, must
  you really scan _all_ the previous indexed documents? That seems
  crazy.

**MinHash**

So, hash all the words in each document; take the minimum. The
probability that the min hash is the same in both documents is equal
to their Jaccard similarity.

Why? First, MinHash defines a permutation of $A \union B$. The minimum
element (call it $z$) is equiprobable across $A \union B$ (this is an
assumed property of the hash function). Now, $MinHash(A) = MinHash(B)$
exactly when $z \in A \intersect B$. Thus we have $Pr[MinHash(A) =
MinHash(B)] = \frac{|A \intersect B|}{|A \union B|}$.

Comparing a single MinHash is an unbiased 0-1 estimator of the Jaccard
similarity, but it has very high variance (because Jaccard similarity is
between zero and one, and MinHash comparison is binary). To reduce
variance of the estimate (get it to converge to the Jaccard similarity),
you can hash with `k` different hashing functions; or maybe just use the
same hashing function with `k` different salts. You then divide the
number of matches by `k` to have an unbiased estimator of the Jaccard
similarity with lower variance.

Because hashing can be expensive, you can try to just pick the smallest
`k` elements with respect to a single hash function. However, this
begins to deviate from optimality. You can sort of

Alternatively, to avoid too much hashing, you can just pick the smallest
`k` elements wrt a single hash. You estimate the Jacard similarity by
how much overlap there is. It turns out this estimate of Jaccard
similarity is _still_ unbiased, and indeed even has lower variance. It
is analogous to estimating the ratio of red/black balls in an urn with
or without replacement; without replacement is the _better_ estimate.
However, if you want to use the hashes for further purposes beyond
Jaccard similarity estimation (e.g., for approximate nearest neighbor
search), you can't exactly treat this as either `k` independent trials,
nor the result of `k` independent trials.

**Min-Wise Independent Hash Functions**

There is another subtlety. What we _really_ want is a "random
permutation" on a bag of words. We are trying to do this by randomly
sampling a hash. But there are `n!` permutations. The space of hash
functions that we sample from should have _at least_ `n!` functions. If
not, we don't even haver enough hash functions to hit every permutation.

Even then, we'd need to assume that every permutation of the bag of
words is equally likely after random choice of hash function. If the
hash function family is susceptible to some kind of regularity in the
input data, this might not be true!

In practice, we use a _salt_ to randomize selection of a hash function.
This works well enough if we assume that hash structure won't interact
poorly with document/word structure. But we would still need `O(n log
n)` bits of salt to select one of `n!` hash functions. That is too much
salt!

In reality, we need something _weaker_. We need that the smallest
element under the hash is uniformly distribute. We don't care about the
order of any higher elements. So there is some research into whether
such hashes exist, under what conditions, and what those hashes are.

This is more of a theoretical issue. It's kind of similar to the
question: "Do good hash functions exist?"

# Hamming Distance/Bit Sampling

Say you want to compare two bitstrings. Your family of hash functions
can just be the selection of a random bit. Then the probability of
collision corresponds to the fraction of the bits in common. That is the
normalized Hamming "similarity".

You might ask: could we have used this instead of MinHash to estimate
Jaccard similarity? That is: take just `k` words from the corpus
vocabulary. For every document, just record the presence/absence of
those `k` words. Use these bit vectors to estimate Jaccard similarity?
But that wouldn't really work. The hamming distance of these bitvectors
is _not_ proportionate to the Jaccard similarity. For instance, lots of
documents would not have _any_ words form the set of `k`, so the
bitvector would be all zeroes. Two zero vectors are identical under
Hamming distance, but don't suggest a Jaccard similarity of 1.

Really, we would need to divide by the sum of the popped bits in the two
vectors to have a proper Jaccard similarity estimate. And that count is
going to be _very_ small. Basically, we're using a lot of space to store
zeroes in the bit vector, which is inefficient for the amount of
precision about the Jaccard similarity we can achieve.

# Cosine Similarity/Random Projection/SimHash

Say you have vectors in a vector space, and you want to find those
vectors with the greatest cosine similarity.

In that case, generate a bunch of hyperplanes (through the origin). The
probability that the two vectors lie on opposite sides of the hyperplane
depends on the angle between them. To calculate which side of the
hyperplane a vector lies on, you take the sign of the vector's dot
product with a vector unit normal to the hyperplane.

Technically, $Pr[sgn(u \cdot r) = sgn(v \cdot r)] = 1 - \frac{\theta(u,
v)}{\pi}$, where $\theta(u, v)$ is the angle between $u, v$. This is not
exactly $\cos(\theta(u, v))$, so it's not quite right to use this as an
estimate of cosine similarity. But I believe it is monotone with
cosine similarity, which is enough.

So, to hash your vectors, you first specify `k` projections. You then
create a hash vector of `k` bits by popping the bit if the feature
vector lies above the hyperplane.

You can now use the _Hamming distance_ of the two hash vectors. The
Hamming similarity should be monotone with cosine similarity. To
estimate angle from Hamming distance of the SimHash vectors, this is
`\theta\hat \approx \pi \frac{d_H}{k}`.

# Euclidean Distance

There even appears to be a way to do this for _Euclidean distance_ of
points. You choose random lines, project points onto the line, and then
divide by a "bucket width" to quantize the length of the vector along
this line. Points nearby in space should tend to land in the same
bucket. I think the algorithm is due to Datar-Immorlica-Indyk-Mirrokni.

- Source: http://infolab.stanford.edu/~ullman/mining/2009/similarity3.pdf
  - Some Power Point lecture slides. Seems _okay_.
- Source: https://web.lums.edu.pk/~imdad/pdfs/CS5312_Notes/CS5312_Notes-14-LSH.pdf
  - A mini-text of about 25 pages. Set in Latex, looks pretty good.

# Approximate Nearest Neighbor Lookup

For a corpus, you're going to hash every document. You'll create a hash
map of hash to document id. For a new document, you hash it, and look it
up in the hash map. If there is a match, you can return this document.

However, this is going to be very noisy. For MinHash: a very similar
document may not match the single (`k=1`) MinHash value. For SimHash,
you're not going to lookup a _single bit_ (`k=1`). But if `k` is large,
then it is very unlikely that the MinHash/SimHash will be a match!

We will basically choose an approach where we can trade off precision
and recall. Say we want 95% _recall_ of 80% Jaccard similarity. We can
compute that if we do six rounds (called "bands") of four trials, then
an 80% Jaccard max should have a 95% change that in at least one round
should all trials be positive.

That suggests that we calculate 24 MinHash values total, "band" into six
groups of four MinHash values, and then create six tables. We do six
lookups, and we take the union of the buckets. These are candidates. We
can probably throw away a lot of the candidates away by looking at the
full hash vector. For instance, if we have four matches, followed by 28
mis-matches, that almost certainly isn't 80% similarity.

What is the _precision_; the ratio of 80%+ similarity documents to
"false positives"? All else equal, adding more bands increases recall
but decreases precision. Adding more hashes per band increases precision
but decreases recall. The number of false positives will depend on the
distribution of similarity in the corpus. If there are a very few 80%+
similar documents in a very large corpus, then you'll tend to have a lot
of false positives. But this also depends on the distribution of
similarity: if some documents are 80%+ similar, but other documents are
only 1% similar, and very little in between, that will help you.

# Feature Hashing

Simple feature hashing just hashes the features and does a
dimensionality reduction. This has an advantage of no storage cost for
dictionaries. Also helpful when you could encounter new features online
and want to do online updates to weights.

Gaurav once claimed that feature hashing can _improve_ performance and
Peter couldn't believe that. I think the idea is that dimensionality
reduction this way can serve as a form of regularization or prior.
Imagine you're doing logistic regression, and then are a bunch of unique
features. Then they can steal all the weight and now you won't use the
other features.

When you feature hash, you will put a certain number of useless features
in with these low-reach features. I feel like maybe this implicitly
performs a Beta prior by throwing in something like pseudocounts. Also,
you are reducing model capacity by giving it fewer parameters it can
tune, which can improve test.

It is a long time ago, but I suspect Peter would have said that, were we
to regularize properly, feature hashing would only be a tradeoff of
feature quality for saving memory on dictionaries.
