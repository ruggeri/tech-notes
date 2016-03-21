## Ch1: Data Mining

* Term Frequency: in a document, count how many times a word occurs,
  and normalize this by the count of the most common word.
    * This handles different lengths of documents.
* Inverse Document Frequency is often defined as the `log(N/n_i)`,
  where `N` is the total number of documents, and `n_i` is the count
  of the number of documents the word occurs in.
* The terms with the highest `TF*IDF` in a document tend to best
  characterize the document.

## Ch2: Map-Reduce and the New Software Stack

* Describes MapReduce. Talks about how it can be used to distribute
  large matrix multiplications, as is needed for PageRank.
    * Some relational queries are useful to do in MapReduce, via some
      framework that translate SQL to MapReduce jobs.
    * Good for big, batch queries.

## Ch3: Finding Similar Items

* Talks about shingling (they do `k`-letter shingles). Then you hash,
  so that you can represent the shingles in a shorter format (though
  this adds some noise).
* Still, this would be very large, so we create a signature via
  minhashing.
* We know that we can recover Jacard similarity from the minhashes.
* Even with the minhash signature, it may be hard to find the most
  similar document, since this involves comparing Jacard similarity
  with every minhash signature. We want faster lookup for the most
  similar document.
* LSH basically says: hash several times. Place the item in each of
  these buckets. If similar documents tend to have the same hash,
  you'll probally find the most similar document in one of these
  buckets.
    * One way to do this is to break the dictionary into subsets, and
      do a minhash only inside the subset, generating multiple
      signatures.
    * You could take the signature to be the 3-minhash: the 3 words
      with lowest hash in that subset. That is pretty specific.
    * If there are a number of subsets, there's a good chance for a
      document with a lot of overlap to get an exact match.
* This stuff is mostly covered in my document specific to LSH.
* Uses:
    * Matching two versions of the same document.
    * A good example might be a fingerprint database.
    * Or finding whether two news articles cover same event.
* They add some stuff about how to find high-precision matches, but it
  isn't that interesting.

## Ch4: Mining Data Streams

* You might keep a sliding window, in case that helps.
* If you want to take a sample of a stream, you could just roll a dice
  per record.
* But if you want to sample groups of things, you can hash that item
  and mask the low bits.
    * This is useful, for instance, if you want to keep the searches
      of 1/10th of users.
    * You hash the user id and see if this is `<=2**32/10`. This way
      you don't need to keep a list of users.
* Filtering a stream is often done with a bloom filter.
* Counting distinct elements can be hard, too. One approach is to hash
  each item, and keep track of the most zeros seen at the end of the
  hash. At the end, if the longest number of zeros we saw was `Z`,
  then our estimate for a distinct count could be `2**Z`.
    * We can improve this by using multiple hash functions.
    * You don't want to directly average these, since outliers will
      have a big impact.
    * But median isn't good either, since you'll never gain resolution
      better than a power of two.
    * One solution is to group estimates into small groups, taking the
      average of each. Then you can find the median, to eliminate
      outlier groups.
* *Moments* of a distribution are `Sum C_i**k`, where `C_i` is the
  count of item `i`, and `k` is for the `k`th-moment.
    * Moment zero is just the number of distinct items.
    * First moment is the number of items.
    * Second moment is a measure of surprise. Basically it says
      more-and-more uneven distributions are more and more weird.
    * Actually, I find that I don't care about this algorithm...
* Say you want the count of an event in the last many `N` items.
    * If you want 100% accuracy, you'll need all `N` most recent
      items.
    * The DGIM algorithm says: keep buckets, each with a count of 1s
      in the bucket, and the timestamp of the most recent
      bucket. You'll only have buckets of size `2**k`.
    * Older buckets will have more stuff, newer buckets will have
      less. When we get three buckets of the same size, we'll combine
      them.
    * To answer a query of how many events in the last `k` items, you
      take all the earlier buckets, and half the count of the last
      bucket before `t-k`.
    * You know that all the more recent buckets are fully contained,
      so error only comes from the earliest bucket, which overlaps the
      time cutoff.
        * But the size of this bucket is the sum of the size of the
          earlier buckets, so splitting down the middle means small
          error.
    * I'm hand-waving here, but you see.
    * You can get higher accuracy by allowing more repeats of buckets.
* They talk about time-decaying events. You could give each event half
  as much weight as the previous, and keep a decayed average at any
  given moment in time.
    * You can do this to do a top-k approach.
    * Each time you see an event, you record its count, time-decayed.
    * You can use a slow time-decay to approximate a large window.
    * You want a cutoff; if the time-decayed value drops below a
      threshold, you cut it.
    * This cutoff implies a maximum count that you will have in
      memory, since not everyone can be above this threshold at the
      same time.

## Ch5: Link Analysis

* Early search engines were just inverted indexes. People term spammed
  to make their pages seem relevant to queries where they were not in
  fact relevant.
* PageRank gives more weight to those pages where, by randomly
  clicking around, they are likely to end up.
    * That's pretty similar to a MCMC idea.
    * PageRank also started using terms that appeared in and near
      links to the document. Spammers had less control over these.
    * Basically, the weight of a document is proportional to the
      weight of documents that refer to it. And the weight given to a
      term is proportional to the document it appears in.
* You can calculate PageRank by defining a transition matrix, where
  the probability of moving from one document to the next is inversely
  proportional to the number of links.
    * Then you start with an equal probability distribution, and just
      apply this matrix repeatedly.
    * You are effectively finding an eigenvalue of this matrix.
* To avoid dead-ends and "spider-traps" (strongly connected components
  with no out edges) getting all the PageRank, we introduce
  "taxation", which is a probability that the user gives up and jumps
  to some other page.
* PageRank is used secondary to the inverted index to order the
  results.
* To compute PageRank, note the transition matrix is sparse. But even
  then it is probably too big to hold in memory.
    * We break the transition matrix into 16ths and the vector into
      4ths.
    * Each mapper is only responsible for apply a 16th of the matrix
      to 1/4 of the vector. This means there are 4 mappers per quarter
      of the vector, for 16 mappers overall.
* We can make PageRank topic-sensitive by biasing the teleportation
  toward a subset of pages that are known to match a topic.
    * Now, when we search, we can try to infer the topic from the
      query, and use the appropriate topic-sensitive PageRank.
* They don't talk too much about how to infer a topic from a
  query. They suggest we can pick out important words from a subject
  by computing the tf-idf. Then we can do a Jacard similarity.
* They talk about how to fight link spam, by changing the teleport set
  to just *trusted* pages, where users can't create links themselves.
    * I think it would be easier just to detect UGC and eliminate
      those links.
* They spend some additional time talking about hubs and authorities,
  but it's boring.

## Ch6: Frequent Itemsets

* Say you want to find those items which tend to be correlated.
* To do this, you might have documents (which are sometimes called
  *baskets*) consisting of words (sometimes called *items*). You want
  to find *itemsets*, which are things that are very likely to co-occur.
* I believe this came from market research, where we wanted to find
  what items were purchased together.
* The number of baskets in which an itemset occurs is called its
  *support*. A typical problem is to find all itemsets with a
  threshold support.
* Goal could be to find related concepts.
* We can also try to find *association rules*, where the presence of
  an itemset makes the presence of yet another item unusually likely.
    * *Confidence* in a proposed rule is the ratio of the support of
      the itemset with the other item appended, to just the itemset's
      support.
    * *Interest* of a rule is the difference between the confidence
      and the base rate of the item.
    * Note, confidence is really misnamed. It is estimating the
      conditional probability. The "confidence" should really be a
      measure of the variance in this estimate...
* Note that finding association rules is pretty easy once we identify
  frequent itemsets. Low frequency itemsets aren't going to have a
  good enough estimate of the conditional probability.
* The A-Priori Algorithm...
