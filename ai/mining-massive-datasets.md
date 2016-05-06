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
    * I think this is kinda like what HyperLogLog does, but with a
      better way of combining estimates.
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
    * But we might be missing out on the relationships between similar
      items, and pooling information...
    * This isn't a very principled approach.
    * It makes more sense for "brick-and-mortar" stores, where they
      can only run so many deals, and can only go for big wins. By not
      looking deeper, could be missing out on micro-optimizations.
* Naive approach:
    * Iterate through baskets.
    * Generate pairs of items in a basket.
    * Keep, in memory, a count for each pair itemset.
    * This won't work if you can't keep all pairs in memory.
        * Note, not *all* the pairs will need to be stored in memory
          (if they don't reach the threshold), but many will...
* The Apriori Algorithm
    * Make a pass, recording every frequent item in a hash set.
    * Make a second pass; count all pairs of frequent items. You are
      eliminating any pairs with *non-frequent* items, which is a huge
      win.
    * You can repeat this in subsequent passes, where you only
      consider itemsets which consists of combinations of things that
      were frequent in the previous pass. This should winnow down the
      number of combinations very quickly.
* This may not cut down enough candidate combinations, if all the
  singletons meet the support requirement, all pairs will be
  counted. Not all of these will meet the support threshold, but
  they couldn't be eliminated a priori.
    * What you can do is, when counting singletons, also count hashes
      of pairs, modulo some number.
    * Now, when considering a candidate, in addition to checking
      whether there is enough support for the two elements, you can
      check the hash.
    * The hash would be modulo some number, so it's a false-positive,
      no-false-negative structure.
    * Doesn't add a second pass pass; can be done along with the
      singleton counts.
* We can *refine* this approach, with multiple passes, if
  necessary. We do that by, after the first hash table is produced,
  creating a smaller summary of those buckets with sufficient support.
    * Then we repeat, with a more specific hash function.
    * We can repeat this, to get several filters.
* They talk about how you could take a random sample, and do one of
  these algorithms on the sample. If you take a 1% sample, you might
  use `0.1s` as your support.
    * This leads to false positives and false negatives.
    * You could use your sample to make a pass through the full data,
      counting only those things flagged as candidates with enough
      support. In this way you can eliminate false positives.
    * You can reduce false negatives by further reducing required
      support to `0.9ps`, in which case you get more false positives,
      but then those get filtered out by your second pass...
* A less random approach kinda like polyphase merge sort:
    * Break file into chunks.
    * Run any of the in-memory algorithms on it. Lower the threshold
      to `s/#chunks`.
    * Take union of all candidates identified in the chunks.
    * Then make a second pass to do the counts.
    * This has no false negatives, since if the count is less than
      `s/#chunks` for every chunk, it can't possibly have sufficient
      support.
* They present this zany algorithm which looks at a sample, computes
  frequent itemsets, and also the "negative border", which is
  basically a list of potential false negatives.
    * The negative border is sets where, deleting one item, you get a
      frequent itemset.
    * If the sample produces any false negatives, then something in
      the negative border is a false negative.
    * This is a Las Vegas algorithm; you run a pass on the whole data,
      keeping counts of the frequent itemsets and negative border.
    * If something in the negative border is frequent, you need to
      repeat with a lower threshold on the sample data. Else you know
      you are good.
* What about with a stream of baskets coming in?
    * We could run this in an "offline" mode; collect data for a day,
      then run a job to find frequent items for this day. Next day
      repeat with the new data.
    * Another way is to start with an offline calculated frequent item
      sets, and track statistics for these. Delete them if they become
      too infrequent. Keep track of the negative border and add these
      if they become common enough.
    * That sounds kinda tricky, but it doesn't matter because decaying
      windows is probably better anyway.
* We've seen using decaying windows for event frequency. How to adjust
  this for itemsets?
    * The stream provides baskets; we can take each subset and add a
      count of one for this itemset.
    * But that doesn't really work, since a basket of 20 items can
      have >1MM subsets! So maybe just track singletons.
    * A trick then is to never add a subset until all its own subsets
      are being counted. This means there could be a little lag, but
      eventually we'll consider adding pairs of items if the singleton
      itemsets are common, and triples when we have both pairs...
    * We'll flush out anyone where the decayed count falls too low.
* The only difficulty is that we might have sorta high memory usage,
  because we need to keep all items with decayed counts `>0.5`.
    * I could see ways to deal with this, where you actually jack the
      threshold up the longer its been in memory.
    * That allows you to get new stuff in the system, but once you
      have good estimates of their counts, to flush them out, too.

## Ch7: Clustering

* We want to discover clusters. This can be done in Euclidean spaces,
  but we may have other spaces:
    * Documents with Jacard similarity metric.
    * Cosine distance.
        * Helpful if repeats of a word should be taken into account.
    * Hamming distance.
        * If you just have a binary value for present/not present.
        * Probably Jaccard is better in a lot of cases, since it's
          sensitive to the number of *present* terms; terms that
          aren't present are considered meaningless.
* Hierarchical clustering starts with everyone in their own cluster,
  then merges.
    * This implicitly builds a tree, by the way.
    * Typical approach starts with `n` clusters, so `n**2` merged
      clusters to consider. You keep repeating this, so overall this
      is `O(n**3)` time. Not very efficient!
    * BTW: my thought is that you could do `k`-means repeatedly, each
      time with `k/2` clusters.
    * Indeed, I think you want to merge clusters by proximity of their
      centroid. Quality of a cluster is distance from centroid.
    * But to stop from over-merging, you might try to use a measure of
      density in the cluster.
    * This feels *really* hacky.
* Interesting problem: when you aren't using Euclidean distance,
  finding a point that is at the "center" of a cluster can be hard.
    * I do feel like the average still works for cosine distance?
    * But for string edit or Hamming distance this would probably be
      really difficult.
    * It sounds like they take the datapoint which has minimum
      distance from the others in the cluster.
    * For merging clusters, you can average distance of all points of
      one cluster to another, to see which one is closer. That sounds
      expensive!
* They discuss several clustering algorithms. Some approaches:
    * BFR: a variant of mixture of gaussians but where covariance is
      assumed to be zero. There's some optimized way to calculate
      this. Presumably you do PCA first...
    * CURE: clusters have representatives, you presumably calculate
      distance to the closest representative when assigning to a
      cluster.
    * GRGPF: for non-Euclidean clustering. Creates tree.
* Clustering streaming data
    * You could have "buckets" of points, and clusters in each bucket.
    * When you merge two buckets, if you're doing k-means, you can
      find the two clusters with the closest centroids, and merge
      those.
        * I think that's the best centroid if we put all those points
          together, but it might not be the best merge of two clusters.
        * Still, it sounds okay.
    * Actually this part of the book is very unclear. But I don't
      care.
* All of this discussion of clustering is extremely hacky and
  unprincipled.

## Ch8: Advertising on the Web

* Adwords:
    * Want to show ads that have good `clickthrough*bid`.
    * Also, there's a certain budget per month.
* Simple model: clickthrough rate is all the same, all advertisers
  have the same budget, and bids are all the same.
    * Then one approach is to assign the ad to the bidder with the
      greatest budget left.
* In a model with different bid amounts, they suggest valuing bids at
  `b(1-e**-f)`, where `f` is the proportion of the budget remaining.
* This problem is also uninteresting!
* How to lookup matches of bids to keywords?
    * The advertiser specifies the set of keywords, we only activate
      their bid if there is an unordered match of keywords searched
      and keywords listed in the bid.
    * Notably, we do not generate all subsets of the keyphrase.
    * But what about bidding on emails? These have many possible
      phrases. How will we match?
* First, sort bids by rarest word first.
* Then, when matching, keep a hash table of bids keyed by first word.
    * For each word in the document, lookup the key, and move
      everything in this bucket to other buckets, based on the *next*
      word.
    * When you get to the end, you can activate the bid.
* Generally this is pretty scalable: you can run many machines
  independently, each handling some of the documents to match, and for
  different subsets of ads.
* To reduce memory, I think you could use a DAWG, and for each
  document keep a hash map of pointers in the DAWG, representing your
  progress through the DAWG.
* Now *that's* an interesting problem!
    * It's kinda like a "reverse" version of a search engine, where
      you have documents, but want to lookup a short query.

## Ch9: Recommendation Systems

* Two kinds:
    * Content-based: try to categorize the documents, and offer more
      documents that are similar to what the user has read.
    * Collaborative filtering: use associations between what users
      have seen to form categories, and also to guess at preference of
      the user for each category.
* Could fill in either by a rating (which can be trouble because of
  bias), or based on what a user has viewed/purchased/looked at.
* Content-Based data:
    * For a movie, could be actors, director, year of release, genre.
    * Could have metadata, like reviews for books, synopsis.
    * For documents, we could pick top words by tf-idf.
    * We could then use minhashing to identify similar documents.
    * We want to recommend to the user, so we might compute a user's
      "ideal" document, which is maybe the average of documents they
      viewed.
    * We can lookup this ideal document in the LSH.
* This feels kind of ad hoc: maybe we should just use machine learning
  to try to predict what things a user would like.
    * But are we going to do that *per user*? That sounds like it
      could take a while?
    * The benefit of the "ideal document" approach is that it is so
      dead simple, and needs one pass through the user data.
* Next idea is to use collaborative, which will help us because
  similar users will tend to like similar things.
    * They then talk about the EM approach to solving collaborative
      filtering.
* I wonder if there is a way to combine both kinds of information.
    * Maybe you could project documents into some topic space, then do
      collaborative filtering on this projection of the data.
    * That seems kinda sub optimal, since you might want to use the
      collaborative data to help with the topic formation.
    * But just as you can do EM alternating between adjusting two
      sides, maybe you can do the same by cycling through three
      variables.

## Ch10: Mining Social-Network Graphs

* Social graphs don't have a good definition of distance. What's the
  distance between two nodes if there is no connection between them?
    * That actually seems natural: the length of the shortest path
      between them...
    * Seems like that defines a true distance metric...
    * However, maybe we should consider people "closer" if they are
      both part of a highly connected subnetwork.
    * The book makes a lot of hay about how having all edges represent
      the same distance is unworkable.
        * For instance, any two clusters that are adjacent appear to
          be at the same distance from each other.
        * But that doesn't seem fair! You could talk about the average
          distance between members of one cluster and the second!
* *Betweenness* is how many shortest paths run through an edge. The
  idea is that, if an edge is inside a cluster, there should be many
  ways to get between nodes in the cluster, meaning the betweenness
  should be low.
    * To calculate, do a BFS from a starting node X. Edges between
      "levels" are part of the shortest path from X to a node. Edges
      inside a level are not part of a shortest path from X.
    * View the BFS as a polytree: we're organizing nodes in levels,
      but a vertex can have multiple parents if it has multiple edges
      from previous level.
    * Let's label nodes with the number of shortest paths they have
      from X. Start with 1 at the root. Let each node be labeled with
      the sum of its parents' labels. This is the number of shortest
      paths from X.
    * Now, to give credit to edges, start by giving leaves a credit of
      one. Split this credit amongst the edges into the leaf
      equally. Then move up a level, giving the next node a credit of
      one plus the sum of the credits on the outgoing edges. Continue
      bubbling. Finito!
    * Repeat with each vertex as the root!
    * That sounds bullshit. We ought be able to do this faster. I
      believe this approach is `O(VE)`, which sucks.
    * One recommendation is to choose a random subset as starting
      points. This gives a fairly good estimate of betweenness.
* Suggestion is: set a ceiling on betweenness, and consider all
  connected components to be in a cluster.
    * One problem: people can't be part of multiple clusters.
    * Also, no generative justification.
* Another idea: given a bipartite graph, you can find complete
  bipartite subgraphs.
    * There's an algorithm to do this.
    * This is a very strong degree of connection.
    * You can use this as a "nucleus" of a group, assigning more
      vertices that are connected to many of the nucleus nodes.
    * Idea is to randomly partition a graph, and then find these
      nuclei.
        * You, for now, ignore edges on the same of the partition.
        * Those will only be used for expanding the nucleus.
    * This idea seems entirely unprincipled to me.
    * I think one thing they like is that finding these complete
      bipartite subgraphs is the same as frequent itemset matching:
      you have "items" on the left, and "baskets" on the right. If a
      vertex on the right is connected to many vertices on the left,
      they are all in the same "basket".
* Graph cut model
    * Want to cut the graph into parts, partitioning it.
    * We count the number of edges stradling a cut, and normalize it
      by dividing by the number of edges with at least one edge in the
      partition (volume). There are two sides of a cut, so we do this
      twice.
    * Define the Laplacian to be the difference between `D`, the
      degree matrix (diagonal matrix with each entry the degree of a
      vertex), and `A`, the adjacent matrix (each entry 1 iff an edge
      between the vertices).
    * They say that finding the eigenvector for the smallest non-zero
      eigenvalue is the next bet cut.
    * TODO: What is the math behind this?? Something something
      spectral theorem?
    * Again, only partitions.
    * I think this is called "spectral clustering". Spectral
      clustering uses
    * He suggests that the eigenvectors of `A` are a labeling which is
      consistent with the labeling of neighbor nodes.
    * He mentions a graph where every vertex has degree `d`. If there
      are two components of the graph, then an eigenvector is to
      assign all the vertices in one a value, and all the vertices in
      the other another value.
    * He notes that this shouldn't change very much if these two
      components happened to be connected by a few edges.
    * Seems like it should still be possible to assign them all the
      same cluster...
    * I think `L=D-A` is sort of normalizing for the number of
      edges. Basically, if our `N(u)` neighbors all vote `x`, and we
      are labeled `x`, then there is zero error at this vertex.
    * Wow, I think there's actually a lot to spectral theory...
* I'm pretty unimpressed by this model. I can't really justify this
  metric. I get that it wants to penalize low internal connectivity
  and high external connectivity. But it just seems so totally
  unprincipled.
    * I guess, after some experience, I don't like these problems
      where you try to define a solution by characteristics of what
      you're trying to model.
    * I think it's better to *propose* a model, even a simple one, and
      then learn that.
* Affiliation Model:
    * Every vertex is part of some groups. Has a certain prob of being
      in a group.
    * If two vertexs are in the same group, they have a probability
      `P_g` of sharing an edge. If they are in multiple groups,
      probability of an edge is `1-\Prod 1-P_{g_i}`. And if not in any
      shared groups: a base probability.
    * Prolly should just make all people in the same group with low
      connectivity for consistency...
* You can have a more general model (called BigCLAM), where vertices
  have a strength of membership. Then probability of an edge from
  group A `P_A(u,v) = 1-exp(-F_uA*F_uB)`. Here `F_uA` is the strength
  of vertex `u`'s membership of group `A`.
    * Now we don't have a parameter for probability of groups
      connectivity.
    * You combine these probabilities the same way.
* But now you have a matrix problem!
    * Rows are the membership strengths for a vertex.
    * To find overall probability, you can take product of `FF\trans`
      to calculate the relationship strength between every two nodes. Then
      you do `1-exp(FF\trans)`.
    * You can verify this is the same as if we calculated `1-\Prod
      1-P_i(u, v)`!
* To solve this, you want to find the matrix `F` that maximizes the
  likelihood of the graph. In particular you want to maximize:

    \Sum (u,v)\in E log(1-exp(-F_uF_v\trans))-\Sum (u,v)\not\in E F_uF_v\trans

* Can solve this using gradient descent.
* One optimization. This requires considering all pairs of vertices
  (edge or not edge). We want to only consider those edges that
  *do* exist.
    * So we sum up the total weight in every group (these are colsums).
    * Then we subtract out the vector representing a node.
    * We then subtract out its neighbors.
    * This is the total weight in each group of its non-neighbors,
      needed for gradient descent.
* So we expect this to be linear in the number of edges, I
  believe. That is a great speedup for sparse graphs!
* There's also an idea, called "simrank". We have a random walker
  start from a vertex. A neighbor is chosen equiprobably. We also
  relocate the walker back to the start vertex with some probability.
* The probability we observe them in any given location represents the
  similarity with the start node.
* They discuss an algorithm for finding clusters of triangles, which
  again could be considered nucleii of clusters. They give a MapReduce
  implementation for this. However, I am uninterested.
* They give an example of finding clusters of search queries:
    * Advertisers bid on search queries
    * You add an edge between the advertiser and these search queries.
    * After clustering, you have the ability to "broaden" search
      queries with other related queries, by virtue of the graphical
      connection with the advertisers.

## Ch11: Dimensionality Reduction

* We already kinda did this by trying to factor a matrix of movie
  scores by users, adding in a lower dimensional set of movie
  categories.
* We showed how we might calculate an eigenvector by power iteration.
* Of course, eigenpairs can be calculated exactly, by solving
  `(M-\lambdaI)v=0`.
    * From the polynomial equation for the determinant, we can
      calculate the `\lambda`, the eigenvalues.
    * Given `\lambda`, we can calculate its eigenspace through regular
      matrix inversion.
* Another approach: use power iteration to approximate the principal
  eigenvector.
    * Then "remove" this from the matrix, subtract this out of each
      column of the matrix. This increases the nullspace, of course.
    * Repeat the process!
* Note, all eigenvectors are orthogonal. A matrix of eigenvectors is
  orthogonal.
* PCA is going to find orthogonal axes along which data varies the
  most. If you project this data onto a subset of these axes, then the
  distance from the actual point should be minimized.
    * Normally we try to approximate a solution to `AX=Y`.
    * Here, I want to use a new representation of the data to predict
      *the data*.
    * Note this silly equation. Say I have an orthonormal basis for
      the space. Then project `X` onto the basis, `Q\transX`. We can
      then map back to `X` via `Q(Q\transX)`.
    * But here's the trick: pick `Q` to be a rectangular matrix, with
      lower dimensionality than the sample space.
    * When you do this, you will no longer be able to recover `X`
      completely.
    * How to pick the best `Q`? You want to choose axes on which `X`
      has the most variation.
* Note that this SVD technique is essentially what collaborative
  filtering is doing.
* One thing to watch out for is that when doing this decomposition, we
  often start with a *sparse* matrix. We do not want to create any
  *dense* matrices. So we use CUR matrix approximation
    * I'm not very sure how this works. I think I really need to
      understand SVD first.

## Ch12: Large-Scale Machine Learning

* Methods they call out:
    * Decision Trees
    * Perceptron
    * Neural Nets
    * Instance-based learning (k-nearest neighbors, basically)
    * SVM
* To parallelize perceptron learning, they suggest rounds, each of
  which uses the same weight vector. Records are produced for
  misclassified results, keyed by feature index. These can be summed,
  and the weight vector adjusted appropriately. Then we can run more
  rounds.
* They're going to do SGD in map reduce for SVM. That would apply to
  other algorithmsm too.
* Nearest Neighbors: they don't talk how to do this.
* They talk about ensembles of trees as a good way to use trees.
    * That's because as you get deeper in any tree, there is little
      data, and it's hard to generalize.
