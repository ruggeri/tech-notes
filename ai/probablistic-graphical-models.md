# Week 1: Introduction

* Graphical representation allows you to explain joint distribution
  over many variables simply proportional to the number of connections
  between the variables.
    * Normally exponentional in number of variables.
* That is, sparse parameterization. Efficient learning.
* Factors
    * Basically a function from some domains to a real number.
    * You can take a product of factors; it's basically a "join",
      where the real values are multiplied.
    * Think if you wanted the join probability distribution.
    * You can do marginalization (`P(X, Y)->P(X)`) and "reduction"
      (`P(X, Y)->P(X|y)`) on factors just as you could on real
      probability distributions.
    * Honestly this is just formalism right now.
* A *Bayesian network* consists of vertices and edges. Each vertex
  contains a conditional probability distribution.
    * We can recover the joint probability by doing the factor product
      of the factors at each vertex.
    * Needs to be a DAG, btw. Anything can be structured as a DAG
      because you're always conditioning on prior values. Even if
      causation could go both ways.
* A graph factorizes a distribution if the factor product recovers the
  distribution.
* PGM allows you to:
    * Reason from causes to their effects
    * Back-reason from effects to their causes.
    * Do "inter-causal reasoning", if a grade is low, but course is
      difficult, than it may not say much about the intelligence. The
      revelation of difficulty, though not dependent on intelligence,
      affects our distribution over intelligence.
    * In particular, the variable difficulty "explains away" the
      grade.
* When does an observation of X affect our belief in Y else:
    * When X is an ancestor or descendent of Y.
    * When X shares a common ancestor (W) to Y.
    * *Not* when X and Y share a common descendent.
    * This is called a *V-Structure*.
* When does an observation of X affect our belief in Y, given we
  already have a set of observations of `Z_i`?
    * When there is a path to or from X to Y not including any `Z_i`.
    * When there is a path to/from a common ancestor W not through
      some `W_i`.
    * Through a v-structure if the center of the v-structure is a
      `W_i`.
    * But a little more subtle. Even if the center of the v-structure
      is not observed, *if a descendent of the v-structure is
      observed*, that could give evidence to the center of the
      v-structure, which means that you actually do have a change in
      belief about Y.
* So an *active trail* is a sequence of vertices where:
    * For each `X_i`, not in the set `Z` of observed vertices.
    * Exception: if `X_i` is a center of a v-structure, must be
      activated by the presence of a vertex in `Z` in its subtree.
* Conditional independence:
    * `P(X, Y)=P(X)P(Y)` and `P(X|Y)=P(X)` (and vice versa). That's
      just regular independence.
    * All are equivalent.
    * `P(X, Y|Z)=P(X|Z)P(Y|Z)` or `P(X|Y, Z)=P(X, Z)`. Both are
      defining conditional independence.
    * Even if X and Y are not independent, they can be conditionally
      independent. Imagine the reuslts of two coin tosses (X and Y),
      which may be conditionally independent on Z, the choice between
      a biased and non-biased coin.
* Interesting: you can *lose independence when conditioning*.
    * This corresponds to v-structures. Intelligence and difficulty
      may be independent, but not when conditioned on grade.
* D-separation given Z:
    * If there is no active trail from X and Y given Z, then X and Y
      are said to be d-separated. Thus they will be conditionally
      independent.
    * In particular, any node is d-separated from non-descendents
      given its parents. You can't approach from above, so you have to
      approach from below. But if we're not talking about a
      descendent, then we have to go through a v-structure. But we
      weren't given any non-parents to condition on, so can't move
      through a v-structure.
* We will say that a graph is an *I-map* for a probability
  distribution if all the independences implied by d-separability
  correspond to conditional independencies of the probability
  distribution.
    * Basically, we're saying the structure of the graph is
      appropriate for the probability distribution.
    * Technically, we allow that not all independences are expressed
      by the graph, only that the graph doesn't imply any independence
      that isn't in the distribution.
* Naive Bayes
    * 1 class, observe some features. All the features are
      conditionally independent on the class.
    * Given this factorization, the odds ratio of the class is equal
      to the base odds times the product of the odds ratios for the
      features conditioned on each class.
    * Bernouli Naive Bayes: 1/0 if a feature appears ever.
    * Multinomial Naive Bayes: each feature is the i-th word in the
      document. So a word could appear multiple times. Again, assumes
      independence given the class. You assume that the probability
      distribution is identical at each of the n features: that the
      dist of the 1st word is same as the last. It just allows you to
      capture repeat words to further boost score.
    * Pretty effective in applications with many weakly relevant
      features. Independence assumption starts to hurt us when many
      features are strongly correlated.
* Application: Medical Diagnosis
    * When hardcoding probabilities into your model, you have to rely
      on experts.
    * Interesting finding: the way you ask for these probabilities is
      important. E.g., asking a doctor about probabilities of
      different findings given a disease was inaccurate, but asking
      for probability of a finding given different diseases was better
      estimated.
    * Of course, those are really the same question, in some sense,
      but we're just better at estimating one way than the other?
* One advantage of bayesian nets is that you can fill in observations
  as you see them; you don't need to observe all the variables to do
  prediction. You can measure more variables and update on the fly.
* Template models: basically, general structures of model. E.g.:
    * Genetic trees.
    * Word models for NLP.
    * Image models
    * We had a student and course model; could have multiple courses
      and multiple students. Easily generalize the structure. Also
      note that many paramaters are shared, which will probably
      improve learning.
    * We'll also learn about time-series models, which often have
      templating.
* Markov models:
    * You assume that the next observation is conditionally
      independent on all prior observations, given the most recent `k`
      observations.
    * If `k=1`, you can't capture the "velocity". You can do this if
      you set `k=2`.
    * But of course these are noisy estimates, so you might want more
      datapoints.
    * These are *semi-markov* models.
    * Another hack is to add a measure of velocity into the previous
      state. Koller wants to take this approach.
    * Assumes the dynamics of the system don't change.
    * A bayesian network that is expressed via a template respecting
      the Markov assumption is called a Dynamic Bayesian Network.
* A simple DBN:
    * HMM: a single hidden variable, and a single observation.
    * A simple state machine can express transitions of the hidden
      variable.
    * Basically relates a markov model (like a model for phones
      corresponding to words), and also observe the audio signal.
    * For audio application, it's not clear to me how to segment
      phones, but that's another question.
    * But the idea is that you have a generative model, but then you
      also have observations so you can move back.
* Plate models:
    * The *plate* is the repeat part. But there are often outside
      parameters.
    * You can *nest* plates, if there are repeated elements within
      something that is itself repeated.
    * You might have *overlapping* plates. That makes sense in course
      and student plates.
    * Basically, plates are a language for describing a limited set of
      graphical models.
* When you have overlap like this, you have the ability to do a kind
  of *collective inference* (which is like collaborative filtering).
    * E.g., many students take the same class, if most do poorly, it's
      a hard class so maybe doesn't speak to their intelligence.
    * So overlapping plates is a way we can gain data to learn.
* I've already started seeing her add parameters as hidden
  variables. I can see the Bayesian direction this is going...

## Week 2

* Don't need to have a tabular representation for a CPD; that would
  grow exponentionally in the number of parents.
* Sometimes, because of the structure of the CPD, there is a
  "context-specific independence".
    * This means that for a given assignment of a variable, then two
      variables are independent. That might not be true for all
      assignments.
    * For instance, if `X=OR(Y1, Y2)`, then Y2 is independent of X
      given `Y1=11`. Likewise, if `X=0`, then Y1 and Y2 are
      independent.
* A common CPD structure might be a decision tree.
    * Depending on the structure, this may not need as many entries as
      the joint distribution.
    * A common decision structure is that there are 3 parents: two
      variables, plus a third that determines which of the two will be
      used for the CPD of the child. The third variable is like a
      switch.
    * Note: with a switch like this, you can have new, non-context
      independencies arrise. For instance, given the response and the
      switch, the two inputs are independent, because the value of one
      can no longer affect the value of the other. It's like a valve
      has been shut off.
    * This switch is sometimes generalized to many inputs, and is
      called a *multiplexer CPD*; the response variable typically just
      takes on the value of one of the parents.
* *Noisy OR* is another common CPD. Basically:
    * For each `X_i`, imagine a feature `Z_i` that takes on 0 if
      `X_i=0` or 1 with prob `lambda_i` if `X_i=1`.
    * Then Y is the OR of the `Z_i`.
    * Typically, to allow that Y may turn on even if none of the `X_i`
      are on, we have a leak variable, `Z_0`, with its own probability
      `lambda_0` of being on, conditional of nothing. We often call
      this a *leak*.
    * So `P(Y=0)=(1-lambda_0)Prod_{X_i=1}(1-lambda_i)`.
    * Interesting note: the `X_i` are independent given `y=0`. That's
      because `y=0` already says that `Z_i=0`, which gives you all the
      info about whether `X_i` is zero or one. The knowledge of other
      `X_i` doesn't tell you anything more, since you already know
      `Z_i`.
* There are analogues to noisy OR like noisy AND or noisy MAX.
* Also a sigmoid CPD; `P(Y=1|{X_i})=sigmoid(theta'X)`.
    * Note about sigmoid function: the odds ratio of the output is
      `e^z`.
* CPDs of course can be continuous. Common case is a normal
  distribution where mean is centered at parent's value. A linear
  gaussian is an extension with many parents and whose mean is a
  linear combination of inputs. The stddev is fixed.
    * Conditional linear gaussian is a variant where the weights (and
      stddev) rely on a variable A.
* Pairwise Markov Network
    * Undirected edges. Influence can flow either way.
    * Because no notion of a CPD, there's an *affinity function*,
      which is a factor, associated with the edge. The difference is
      that it's not a probability distribution. It's an unnormalized
      distribution.
    * You can still recover a join probability distribution by taking
      the product of factors and normalizing.
    * Note that affinities are *not* probabilities, conditional or
      otherwise.
* Note that pairwise Markov networks cannot express *all* joint
  distributions. This is because there is a max of `O(n**2 d**2)`
  entries in a fully conntected Markov network with domain cardinality
  `d`.
* So we extend; we say that there are factors, which can have multiple
  variables involved. We still write out an edge in the graph if the
  two vertices are in a factor together.
    * This now can express any joint distribution.
    * This is the more general idea of a Markov Random Field.
    * Not sure what the graph structure will buy us. In particular,
      one graph could be the result of many different factorizations.
* Active trails are any path in the MRF. Also, any variable we
  condition on would break an active trail if the path crosses it.
    * TODO2: Even in v-structures?
* Conditional Random Fields
    * Naive Bayes can fall short if the features are not truly
      independent.
    * We can fix that by adding edges, but this adds model complexity,
      meaning we encounter high variance estimates of the weights,
      plus high memory costs for the parameters.
    * Conditional Random Field tries to model just `P(Y|X)`, not `P(X,
      Y)`. Hopefully this simplifies the structure.
    * It's the same as a Markove Random Field, just that we
      marginalize over `Y` and divide that in the partition fn.
* CRFs naturally track a logistic model. If you have binary variables,
  and you have factors of `X_i, Y`, st `phi(X_i, Y)=exp(w_i *
  (1=X_i=Y))`. Then the unnormalized prob of `P(X,
  Y=1)=exp(sum(w_iX_i))`, while `P(X,Y=0)=1`. Last, `P(Y=1|X)` is a
  logistic transform of linear transform of the `X`s.
    * How is this different from NB? She says the correlations don't
      matter?
    * I know NB and LR are different in how they're trained (even
      though they both have the same decision boundary type). The LR
      doesn't try to find the conditional probabilities like NB,
      except for the target variable. NB tries to find each
      `P(X_i|Y)`. Instead, LR tries to find those weights directly to
      help calculate `P(Y|X_i)`. If features aren't independent, it
      can lower the weights on those features to reduce overcounting.
    * Is this the kind of thing she means?
* Obviously we want the sparsest representation of P, therefore we
  want a graph which captures as many of the independencies of P.
    * There is a notion of minimal I-map, but a graph that can't have
      any edges removed might still not be a globally sparsest graph.
    * I.e., just removing redundant edges might get us stuck in a
      local optima.
    * A perfect I-map captures all the independence statements of
      P. But this can be hard to find, and *it might not exist*.
    * Perfect maps are not necessarily unique in Bayesian networks
* TODO2: Converting between Bayes Net and Markov Net might lose
  independencies. What things are more natural to represent as a
  Markov Net?
    * Bayes net => Markov net loses independence in V-structures.
    * Markov net => Bayes net requires us to add *triangulating edges*
      to loops.
* Further:
    * Conditional Random Field is a specific kind of MRF where every
      factor touches a `Y_i`. That means you don't need to know
      relationships between `X` variables except insofar as they touch
      on a `Y` variable.
    * This is easier to specify and encode than a Markov Random
      Network that tried to capture all relationships between `X`
      variables.
* Log-Linear representation
    * Rather than taking the product of factors, we exponentiate a
      linear function of features.
    * Each feature is a linear model of `(X_i, X_2)`. In particular, a
      feature has 4 weights if all factors are of two binary variables.
    * In that case, this log-linear model is just an alternative
      representation with the full expressiveness of the original
      markov net.
* Ising Model is one such model
    * Features are the spin (1, -1) of an electron multiplied by spin
      of adjacent electrons.
    * Affinity is for having same spins.
    * Note how simple "affinity" concept yields a probability of
      settings.
    * This is an example of a metric MRF, where all variables take on
      same domain and there's a notion of similarity.
* Metrics include:
    * 1 if diff, 0 if same.
    * Linear penalty
    * Truncated linear penalty (where a max is imposed)
* BTW: I think in Markov nets an edge appears whenever there is an
  affinity between two thigns. But for 3-way affinities, I think that
  just means adding three edges, so the presence of the edges doesn't
  say what the correct factorization is, I think.
* Example of denoising:
    * Some gaussian noise is added to a photo.
    * You have a cleaned pixel `Y_i`, which has some positive affinity
      to have the same value of the original pixel `X_i`. But you also
      given it positive affinity toward values of the adjacent pixels.
* Oftentimes features are repeated. For instance, adjacent vertices in
  Ising model.
    * We'll use the same weight for every such feature.

## Week 3

* Different architectures:
    * Template-Based vs Specific.
        * Medical Diagnosis often has very specific variables.
        * Image segmentation typically has just a very general
          template-based model.
    * Discriminative vs Generative
        * Discriminative: particular application in mind (what you'll
          use for discrimination). Rich features, want to avoid
          correlations between features.
        * Generative: no predetermined task. Easier to train when you
          have unlabeled data. Another: generative model can be better
          when you don't always observe all the `X`s; think about a
          logistic regression where you don't always observe all
          `X`s. Two variables may be correlated, LR would reduced both
          weights, but what if you see one and not the other?
    * Directed vs Undirected (she didn't explain why you might prefer
      one to another).
* Choosing variables:
    * Some are the target; the things you want to predict. Others are
      observed. Latent, hidden variables.
    * Latent variables help because they can greatly simplify the
      structure of the model, and reduce parameters.
* Structure
    * You can always structure a BN in an arbitrary ordering.
    * But by not orienting arrows wrt causality, you're going to end
      up with a more complex graph and more parameters. Ordering
      things causally will increase sparseness of the graph.
* Tabular CPDs are typically not used. Typically used structured CPDs.
    * Discrete: often trees if the CPD is context specific. If the
      variable is affected by a variety of factors, might use sigmoid
      or noisy or.
    * Continuous: For context-specific continuous CPDs, we might use a
      "regression tree" (I think this is just a tree that tells you
      what regression to use). Linear Gaussian might be good for
      aggregating continuous variables.
* Improving your model:
    * *Sensitivity analysis* can find out what parameters are most
      influential, so that we can focus on them.
    * Do error analysis to add features/dependencies.
* Query is of form `P(Y|E=e)`. NP-hard!
    * Calculating the marginal probability `P(X=x)` exactly is itself
      NP-hard. Even proving whether `P(X=x)>0` is itself NP-hard!
    * Even trying to approximate `P(X=x)` with `eps<0.5` is NP-hard!
      Of course if `eps=0.5`, you could just always guess
      `P(X=x)=0.5`.
    * But these are *worst case* results. So we should start dreaming
      about (1) favorable graph structures and (2) probabilistic
      algorithms, which could work pretty well often.
* Simplest Inference approach:
    * Do "sum product" algorithm of multiplying factors and summing
      out to marginalize based on the evidence.
    * You're pretty fucked because you're going to have to compute
      almost the entire joint distribution.
    * In a markov net will still need to divide by the partition
      function.
        * If you have no evidence (thus calculating entire joint
          distribution), you can renormalize (in effect, you've find
          the partition function).
        * If you have evidedence, I think you at least need to compute
          `P(E=e)`, so you can divide `P(Y and E)`.
    * Note that you'll reduce factors before multiplying in the
      sum-product algorithm.
    * Note, `P(Y|E=e)` involves `Sum_W P(Y, W, E=e)` and `Sum_{Y, W}
      P(Y, W, E=e)`. It's easy to calculate `P(Y, W, E)` if you
      specify all the variables. The problem is that `W` might be very
      large.
* Algorithms:
    * We'll learn about these in the future:
    * Variable Elimination (exact inference).
    * Belief propagation.
    * Variational approximations.
    * Random sampling: MCMC and Importance sampling
* MAP
    * Given evidence, you want the `y` that maximizes `P(Y=y, E=e)`.
    * It's required `Y=X-E`. You do it for *all variables*.
    * MAP problems could be: what's the message sent over a noisy
      channel, what's the segmentation of an image.
    * MAP is *not* the max marginal. Even when you have no
      evidence. That's because you want a single assignment over all
      variables.
    * MAP is NP-hard: finding MAP maximizing probability is
      NP-hard. Even finding whether there exists an assignment
      satisfying `P(x)>p` is NP-hard.
    * To solve, you do a *max product*. Kind of like the *sum product*
      problem.
    * To solve, you want to find `y` maximizing `P(Y|E=e)`. This is
      equivalent to maximizing `P(Y=e, E=e)` (no normalizing
      constant), which is itself equivalent to maximizng the reduced
      factor product.
    * Again, she lists some algorithms:
        * Variable elimination
        * Max-product belief propagation
        * Integer programming techniques
        * Graph-cut, combinatorial search
        * She doesn't describe any of these, really.
* Variable Elimination
    * Given `A->B->C->D`, to find `P(D)` this is:
    * `Sum_{a,b,c} phi(d,c)phi(c,b)phi(b,a)phi(a)`
    * But this is `Sum_{b,c} phi(d,c)phi(c,b)(Sum_a phi(b,a)phi(a))`
    * So we set `tau_1(b) := Sum_a phi(b, a)phi(a)`.
    * That's a factor product (which takes time proportional to the
      product of their CPD size). We then sum out all the values of a
      entries, taking time linear in the product's size.
    * You can treat `tau_1(b)` as a new factor; it's like we've
      eliminate `A` from the model.
    * You can continue likewise: `tau_2(c) := Sum_b phi(c,
      b)tau_1(b)`.
    * To see this through in a linear graph like this, you're talking
      about `O(domain_size**2)` work in multiplying factors, for each
      of `|V|-1` variables.
    * What about when there are multiple children to an eliminated
      variable?
    * Consider `A->B, A->C, B->D, C->D` (diamond). Say we want `p(D)`.
    * Then to eliminate `A`, we need to create `tau_1(B, C)`. Note
      that this is because `B` and `C` are *not independent*.
    * But of course you can do this.
    * What if you have parents? Well, say you tried to eliminate `B`
      first. Then you'd have a new factor `tau_1(A, D)`.
    * In fact, you can eliminate in any order.
    * You want to eliminate in an order that keeps the size of the
      `tau_1` factors from getting to big. Otherwise you have
      exponential blowup.
    * Note that this *may not be topo order*. For instance, consider
      `A->B_i->C`. If you eliminate all the `B_i` first, you get a
      bunch of binary factors `tau_i(A, C)`, each of size two. Then
      eliminating the `tau_i` involves many terms, but each only has
      four entries.
        * Note that eliminating `A` first would be *very painful*,
          since you'd create a huge factor over `{B_i}`.
        * NB: The above should be a **pairwise Markov
          network**. Otherwise, you have to be careful about
          intercausal reasoning through `C` to another `B_j` when you
          eliminate `B_i`.
    * But in a line, you might as well do it in topo order. That would
      be linear in the number of variables (quadratic in domain size).
    * Sometimes you can ignore factors that sum out to one. I think
      anyone downstream of the set `Y` you're trying to find `P(Y)` of
      cannot impact `Y`.
    * I think some naive approaches won't notice this, but that sounds
      kind of silly. It seems really useful.
    * I think maybe it's tricky to find these relationships when you
      *observe evidence*, because with the evidence the old
      independence assumptions embedded in the graph may not apply.
    * Of course, with evidence, you just reduce the factors first.
    * To get the conditional probability `P(Y|E=e)`.
* We can apply Variable Elimination to MN's too. Just need to
  renormalize at the end.
* Basically, the idea is: try to minimize the number of variables in
  each factor you introduce.
* If the *Markov blanket* of a network are those nodes that, given,
  separate the variable from everyone else (basically, your parents,
  your chidlren, and parents of your children), you want to eliminate
  variables such that Markov blanket each time is smallest. At least,
  that's a greedy approach.
    * One way to approach this is to *moralize* the graph. Turn it
      into an undirected graph, where the parents of a vertex have an
      undirected edge added between them. This *marries* the parents
      of children.
    * When you eliminate, you need to connect all adjacent vertices
      into a clique.
    * The *induced graph* is, the result of moralization, followed by
      fill edges introduced by a particular ordering of elimination.
    * Note the induced graph is a function of elimination ordering.
    * One way to think about the induced graph: two vertices have an
      undirected edge if they were ever in the same factor.
    * Every produced factor corresponds to some clique (which are by
      definition maximal) in the induced graph (because eliminating a
      variable means you need to connect all its neighbors.
    * Likewise, think of a clique in the graph. One of the variables
      will have to have been eliminated first. Then this means this
      clique has a corresponding factor that was the result of that
      elimination.
    * Thus, we say that the width of the induced graph is the largest
      clique size, minus one. That is exactly the number of variables
      involved in the largest produced factor.
    * The minimal induced width is the minimal width over all possible
      orderings of elimination. That's what tells us the optimal
      ordering of elimination.
* As always, it's NP-hard to find whether an elimination ordering
  exists with induced width `<K`.
    * But there are simple greedy approaches that work well.
    * Pick minimum number of neighbors in the (undirected) graph. Or
      the minimum size of the formed factor (called *min-weight*);
      that effectively weights the neighbors by size of their domains
      (useful if some variables have greatly different domain
      sizes). Or you could pick such that the minimum number of fill
      edges are added (*min-fill*); basically, you're saying its not
      bad to make a big factor *if you're going to have to do that
      anyway*. You can even do a weighted version of min-fill.
    * **TODO2**: Interesting problem: navigation with landmarks. Have
      robot move with some Gaussian nois. Holy shit that would be cool
      as fuck.
* Alternative to variable elimination: belief propagation
    * We're going to form clusters. Each cluster is responsible for a
      set of factors (each factor managed by one cluster). The cluster
      is connected with any other cluster that has factors with
      overlapping variables.
    * Two clusters will communicate about the variables in the
      intersection of their scopes (the scope is the union of all
      variables in its factors).
        * Hold it! Any two factors need to be able to communicate,
          possibly via intermediaries, about `B`, but don't need to be
          directly connected. If that were required, than naturally if
          three clusters shared a variable, there would be a loops.
    * We initialize all messages to 1.
    * In the algorithm, each turn we select an edge, and reset the
      message to be the product of the initial factors, times the
      product of the messages, but reducing to just those variables
      that the two clusters are communicating about.
    * We continue to do this repeatedly.
    * We can then at the end of the algorithm, talk about each
      clusters belief, which is just the product of its incoming
      messages times its initial belief.
    * Every cluster is going to have its own beliefs. I guess ideally
      if you want to find `P(Y)` for some response variables, you
      create a cluster of just those `Y`s, with an initial factor of
      all 1s?
        * I actually feel like Wikipedia's description seems superior.
    * We'll talk later about how many iterations to run, or how to
      select edges (better than random, prolly?).
* Belief propagation will converge after a while, but it isn't
  always accurate.
* Running Intersection Property
    * If two clusters share a variable, there needs to be exactly one
      path between them containing `X`. Otherwise, the same belief
      will be sent twice between them.
    * In fact, you'll end up in a feedback cycle.
    * Actually, feedback can still be a problem. If you pass a belief
      about X from A to B, but B thinks X is super correlated with Y,
      and sends Y (through some intermediaries), it creates a feedback
      cycle. And indeed BP tends to do poorly with strongly correlated
      features.
    * Anyway, Running Intersection implies that all edges involving a
      variable form a tree.
* She explains it.
    * You have clusters for each factor.
    * You connect each to a cluster representing the single variable.
    * This implies a tree, with the single variable at the root.
* We say that we have acheived *calibration* when adjacent clusters
  agree on a variable.
    * When BP has converged, then the system is *calibrated*. That
      makes sense, since convergence means the messages stop changing.

## Week 4: BP II

* BP is correct in a chain (where it is effectively variable
  elimination.
* BP also works in trees (which are kind of a generalization of a
  chain).
* The idea is going to be that this also works in a clique tree. We're
  effectively doing variable elimination, and thus get an exact
  answer.
* Message passing in trees can be very fast if you work from top to
  bottom, then back up.
    * For chains this is called the forward-backward algorithm.
* To get a marginal probability, you sum out irrelevant variables, and
  then do message passing. Likewise, you can reduce factors to get
  conditional distributions.
* In particular, if the variables you want to reduce or sum out are in
  a single factor, you can just do this at the node, and use the
  message passing you've already computed.
    * If they're in different cliques, you need to update messages in
      the path connecting them.
* Variable elimination is basically segmenting into cliques, and the
  messages that are sent are the messages that would be sent by BP.
    * VE will generate redundant cliques.
* Synchronous BP converges slower than async, typically. That makes
  sense, think about trees.
* Order of message passing can affect both time to convergence, plus
  extent of convergence.
* Common message schedules:
    * Tree reparamaterization (TRP)
        * Pass along a tree. Keep picking trees until you've covered
          all edges.
        * It's ideal to choose trees as large as possible.
        * I guess this helps keep loops from getting too whack?
    * Residual Belief Prop (RBP)
        * Try to pass messages first between clusters that most
          disagree.
    * Common to use *smoothing* (AKA *damping*). Basically, when
      updaing an edge, choose a weighted average of the update and the
      old.
    * The idea is to dampen out the oscillations.
* Of course, you can still converge to the *wrong answer*. That's very
  possible.
* Problems tend to arrise when:
    * You have strong potentials.
    * In tight loops.
    * Giving conflicting signals.
* Related topic: turbocodes.
    * Basically, encode using two methods.
    * Then, when decoding, feed beliefs from one decoder into the
      other.
    * Then repeat.
    * This is loopy BP, basically.
    * This made people start to realize that loopy BP was not
      necessarily a bad idea.
* MAP
    * You can do belief prob to find a MAP assignment.
    * Have to be just a little careful when there are multiple MAP
      assignments. Have to make sure you pick corresponding entries
      from each cluster.
* In the programming assignment they gave me `CreateCliqueTree`, which
  is kinda annoying. I wanted to do that!

## Week 5: Sampling Methods

* Basically one lecture talks a lot about how MAP inference is
  tractable in a lot of scenarios other than graphs with low
  tree-width (the ones that can be efficiently turned into
  clique-trees).
    * One example she gave is "associative potentials", where
      potentials are arranged so that vertices want to take on the
      same value as their neighbor. These can be MAP solved exactly in
      arbitrary graphs with binary cardinality (she claims), and can
      be approximated efficiently with non-binary variables.
    * This comes up in a lot of denoising problems.
    * Another good one: sparse factors (where all the values of the
      factor are the same, except for a sparse subset). This comes up
      when looking for image "patches".
    * Convexity: basically says that a convex set of variables must be
      turned on. This comes up in image segmentation, unsurprisingly.
    * Interesting note: MAP is tractable in a lot of special cases,
      while marginal computation is still intractable in many!
* To do MAP generally, you want:
    * `MAP(theta)=max(sum(theta_i(x_i)) + sum(theta_F(x_F)))`.
        * Where `theta_i` are singleton factors, and `theta_F` are
          multi-var factors.
    * You can't try to maximize these two sums independently, because
      the `x_i` might be inconsistent.
    * This is where you use Lagrangian multipliers; you basically add
      a penalty if the two don't agree.
    * To do, you initialize all `lambda` to be zero. Then you argmax
      `x`. Then you update `lambda` by adding some more incentive for
      these terms to agree (the ones which do disagree).
    * Repeating this, you will find that the `lambda` converge;
      necessary conditions are that the sum of lambda diverges, but
      their squares converge.
    * At converge, each slave has locally optimal solution over own
      variables. But may not be an optimum for the problem if not
      agrees on shared variables.
    * If they diverge, people often try to use several heuristics to
      merge, taking the best answer they found.
    * This entire lecture has been complete bullshit and taught in a
      terrible, garbage way.
* Sampling Methods
    * Empirical expectation will estimate the expectation.
    * Hoeffding Bound:
        * We take `M` samples from `P` and get an empirical
          expectation `T`.
        * Say that the true expectation is `p`.
        * `P(T \not\in [p-\eps, p+\eps])` is the prob that our
          estimate is more than `\eps` wrong from the true
          expectation.
        * Hoeffding says that this is `<= 2 e**{-2M\eps**2}`.
        * That is, the probability decreases rapidly in the number of
          samples `M`, but increases if we needs a tighter `\eps`.
        * Hoeffding is an *additive* bound. For a multiplicative
          bound, we want `P(T \not\in [p(1-\eps), p(1+\eps)])`
        * Chernoff says this is `<= 2 e**{-Mp(\eps**2)/3}`.
    * Anyway, Hoeffding says that if we want to be within additive
      error `\eps` of the truth with a prob > `1-\delta`, we need
      `M>log(2/delta)/2\eps**2`.
    * For a multiplicative bound (via Chernoff), we need
      `M>3log(2/delta)/(p\eps**2)`.
        * But we don't know what `p` is beforehand...
    * In any case, when `p` is small, we need a lot of data.
    * To estimate the joint distribution, you can do sampling of the
      variables in a Bayesian Network in *topological order*. This is
      called *forward sampling*.
    * But we want to do queries with evidence, find `P(Y|x)`. We can
      do forward sampling, but simply throw away samples which
      disagree with our evidence. That seems wasteful, since we're
      generating samples we don't use.
        * This is called *rejection sampling*.
        * This is a bigger problem when evidence is not at the roots.
        * If it's at the root, this is simple.
        * In particular, the proportion of samples kept is equal to
          `P(E=e)`.
        * So we need to do better.
    * Another problem: Markov Networks don't even have a notion of
      roots, so forward sampling doesn't even make sense for MNs.
* MCMC
    * Markov Chain Monte Carlo
    * So a Markov chain is defined by a transition probability
      `T(x->x')`.
    * We repeat this process a number of times. Probability of a state
      at time `t+1` is `P^(t+1)(x')=\Sum_x P^t(X^t=x)T(x->x')`.
    * Typically, `lim_{t->\inf} P^(t+1)(x') - P^t(x)=0`.
    * Hopefully the process converges; if it does, we call the limit
      the *stationary distribution*. We call this `pi`.
        * `pi(x)=prob_of_stating*pi(x) + \sum_neighbors
          prob_of_neighbor*prob_transition`.
        * Basically, stationary probability is the probability you
          would stay here, plus probability that your neighbors
          transition to you.
    * A *regular* markov chain satisfies the probability that there is
      some maximum distance in the chain; that you can always get
      between two states (with prob>0) within a fixed number of steps.
    * A regular Markov chain will converges to a unique stationary
      distribution regardless of start state.
    * Sufficient conditions for regularity:
        * All states are connected by some path with probability >0.
        * And every state has a self-loop. That is, that you can stay.
    * The second condition is necessary. Otherwise, consider if you
      had three states, none of which let you stop. Then in two jumps,
      it is not possible to get to your neighbor.
        * Basically, if there are self-loops, you can set `k` to be
          the diameter of the graph.
    * Basically, to sample from a complex distribution, you want to
      find a Markov chain which would coverge to that distribution.
    * Then you burn-in to try to approach convergence.
    * How do you know it has burned in?
        * One way is to use two initializations. Then after a number
          of steps, you look at the probabilities of states within
          some window of samples.
        * If the probabilities are highly correlated, you may feel
          confident that the chain has *mixed*.
        * NB: you don't want to just look at two windows *in the same
          chain*. That's because there might be hard to jump from one
          region of the graph to another, and you might not be seeing
          any probability of moving into the other region.
    * Once you've burned in, you have that a sample `x^t` has
      probability equal to the stationary distribution.
        * To generate more samples, you can't just take successive
          samples, because they could be correlated.
        * So you *thin*, dropping samples.
        * Koller says you shouldn't thin, though acknowledges that the
          samples aren't IID.
    * Pros/Cons
        * Very general purpose. Simple to implement. Good guarantees
          as `t->\inf`.
        * But lots of tuning params, can be slow to converge, hard to
          see if it really is working.
* Gibbs Sampling
    * Basically we want to use MCMC for graphical models. But what is
      the Markov chain to do the sampling from?
    * The state space for the chain is the sample space; assignments
      to every variable.
    * Given `x`, transition to `x'` according to so:
        * Set `x'_i ~ P(X_i|X_{-i}=x_{-i})`.
        * That is, pick each `x'_i` distributed based on the
          conditional probability of `X_i` given all the other
          previous `x_j` (j not equal to i).
        * Oh, actually, as we sample `x'_i`, we condition on the
          `x'_j` (`j<i`) and the `x_k` (`k>i`). I guess in that way we
          ensure that there is at least some internal consistency to
          the next sample.
    * To sample each `x'_i`, we need only examine those factors
      involving the sampled variable.
* But is the Gibbs chain regular?
    * Not always (fuck!).
    * XOR gives an example, say `Y=XOR(X1, X2)`. You observe
      `Y=1`. You start with `X1=1`, `X2=0`. But the every sample will
      stay the same.
    * However, if all factors are positive, it turns out the Gibbs
      chain *is* regular.
    * So we could fix this problem by giving a very small probability
      that you can allow `X1=X2=1`. But then your mixing will take a
      really, really long time.
    * As a practical matter, slow to mix chains are a real problem.
    * It sounds like there is an alternative to Gibbs sampling which
      can mix faster!
* Reversible chain:
    * Say that the probability of transitioning `x->x'`, weighted by
      `pi(x)`, equals the probability of transitioning `x'->x`,
      weighted by `pi(x')`.
    * This implies that `pi` is the stationary distribution.
    * I guess this is another way of expressing the definition of a
      stationary distribution?
* Metropolis-Hastings has two steps:
    * Proposal distribution of `Q(x->x')`. Acceptance probability
      `A(x->x')`.
    * Algorithm proposes a step, and then makes it if accepted.
    * She then shows that the proper way to pick `A` is to make sure
      that we satisfy the reversability criteria above.
    * The way to do that is set `A(x->x')=min(1,
      pi(x')Q(x'->x)/pi(x)Q(x->x'))`.
    * I'm sorry, I don't understand at all how you find `Q` and `A`
      so as to generate a *desired* stable distribution.
* Metropolis-Hastings Take II
    * I think the idea is that `pi(x)` and `pi(x')` are known. This
      can be found by multiplying the factors appropriately.
    * The idea is that sampling can be hard. In a Bayesian Net, it's
      easy to find the joint distribution entry for *one* factor. But
      it's intractable to find the *entire* joint distribution.
    * You might try to sample by starting at the roots and working
      your way down. But what if you have observed at the bottom? That
      changes the marginal distributions that would live at the
      top. So you can't typically do that.
    * I think this is the idea!

## Week 6: DBNs/Inference Wrapup, Decision Theory and Learning

* In a dynamic bayesian network, we could always unroll and use our
  inference techniques from before.
* But we also know that, given the previous time's state, the
  distribution over the current time's state is independent of
  previous observations. In fact, we just need our current observation
  and the previous belief about the state?
* In detail:
    * We want `P(S^(t+1)|o^(1:(t+1)))`.
    * We use Bayes' Rule to `o^(t+1)`. This decomposes into three
      factors:
        1. `P(o^(t+1)|S^(t+1), o^(1:t))`. But `o^(t+1)` is independent
           of `o^(1:t)` given `S^(t+1)`, so this is
           `P(o^(t+1)|S^(t+1))`. That is easy to compute.
        2. We must calculate `P(S^(t+1)|o^(1:t))`. But that is really
           just equal to `P(S^(t+1)|S^t)` times
           `P(S^t|o^(1:t))`. Which is the previous time step's version
           of the current problem.
        3. We multiply these two, and then need to normalize (by
           `P(o^(t+1)|o^(1:t))`). But that is just a normalizing
           constant and does not need to be explicitly calculated.
    * So basically, we take our beliefs about `S^(t+1)` given `S^(t)`,
      and then reweight them based on the probability that the
      possible states of `S^(t+1)` would have produced the observation.
* Bad news: this approach requires you to keep a joint distribution
  for all the hidden vars, which can become quite difficult. If all
  the variables are *entangled* (that means, that eventually every
  `X_i` at a given time can affect every `X_j` at some future time),
  then there is no conditional independencies amongst hidden vars,
  thus requiring the joint.
* Therefore, approximate methods might be necessary, but I don't know
  anything about that (nothing was taught about it).
    * I bet Kalman Filters would be helpful here.

* Expected Utility: we have some choice nodes, which are not
  probabilistic. We want to choose these, so that expected utility is
  maximized.
    * BTW: you may frequently have multiple utility nodes,
      deterministic in the values of their parents, because this
      allows for factorization of the utility function, easing its
      representation.
    * We often put an edge from variables visible to the agent to
      their decision.
    * The decision rule is a CPD for decision making given the
      observations. Why a CPD, why not deterministic? Possibly because
      of game theory...
    * Once you formulate the decision rule you can calculate expected
      utility even before playing the game.
* To solve, we try to reduce to `Sum_{parents, action}
  delta(action|parents)mu(parents, action)`.
    * That basically means calculate expected utility for each pair of
      parents and each action.
    * Then for each of the parents, pick the action with the maximum
      expected utility. Let that be your delta.
* There was some discussion of utility curves, which was not
  particularly interesting.

* Learning! Finally!
    * Eliciation of expert knowledge can be combined with machine
      learning from data.
    * Want to learn structure and CPDs.
    * Input data can be incomplete. Especially when there are hidden
      factors.
    * One approach: maximum likelihood. But what we really want
      accuracy on new data. Which is separate test data.
    * We may also have a specific structure of query. We observe some
      `X`s and want to predict `Y`. This gets into
      generative/discriminative.
    * Note though, that max-likelihood gives a clear mathematical
      framework for optimization. So we often do that.
    * Other times we just want to discover the structure. The problem
      is then about *knowledge discovery*.
        * Direct vs indirect causes. Direction of edges
          (causality). Presence of hidden factors.
    * Strucuture is a new dimension of overfitting.
* Why PGMs
    * Better than generic approaches when dealing with *structured
      objects*.
    * This means it can exploit correlations between variables.
    * Inject prior knowledge.
    * Learning a generative model, useful for many tasks.

**Book Notes**

* Importance sampling:
    * Basically, you have a proposal distribution `Q`, you weight the
      samples by `P(x)/Q(x)`. This is an unbiased estimator of the
      expected value of X.
    * `P(x)` may not itself be known; maybe only up to a normalizing
      constant. That is common in MRFs. To do **normalized importance
      sampling**, you assign weights `w(x)=P\tilde(x)/Q(x)`. Then you
      take `\Sum f(x)w(x)/ \Sum w(x)`. This normalizes the estimate.
    * Importance sampling for an uncoditional network can reduce
      variance in an estimate. So you might prefer to forward
      sampling.
    * But conditional queries are prolly the real reason why
      importance sampling is important. To do a conditional query,
      "mutilate" the network. Then do forward sampling in this
      network. Then weight the sampling by the ratio of (1) the
      probability of the sample in the original network and (2) the
      probability in the mutilated network.
* But remember: the problem is that your samples from the proposal
  distribution might not be very likely in the target distribution. So
  even though you are normalizing, these samples all have low
  likelihood. Thus you have a very noisy estimate, even if it is
  unbiased. Basically: you're not rejecting samples, but they're not
  providing much value, either. That's why we do Gibbs or MCMC
  sampling; to try to sample from the fat part of the distribution.
* Collapsed particles:
    * Basically, if you have some nodes that are causing trouble, you
      do a sampling technique on just them. The other variables you
      have a closed form conditional probability.
* Smoothing vs Tracking
    * Tracking is belief state given all prior samples.
    * Smoothing is belief state given all prior and some later
      samples.
    * Obv smoothing is not available in real time.

## Week 7: Learning

* Talk about max likelihood, sufficient statistics.
* For max likelihood in a BN:
    * The likelihood is `Prod_{x\in X, u\in scope(PA_x)} Theta_{X=x |
      U=u}^{Count(X=x|U=u}`.
    * In words. Basically, you're saying that you should set each
      entry of theta so that for each possible setting of parents, the
      probability of producing a given `x` is proportional to the
      number of times you saw this.
    * Basically, you're saying that each parent setup defines a
      different bernouli experiment. And your choice of theta for that
      should mimic the results observed in the data.
    * Interesting. That's very general, very simple. But that's for
      *table CPDs*.
    * It's not going to scale as the number of parents gets large
      (since cpds will get huge), and it's not going to display any
      kind of *stability*.
* She mentioned learning HMMs, but that went so fast, and it wasn't
  clear how it would be done. WTF?
* She makes a note: the curse of dimensionality means that models with
  fewer edges between vertices are easier to learn. And, in practice,
  even when edges that are logically supposed to be there are
  nonetheless removed, sometimes we see better generalization.
* In Bayesian approach, we just weight the likelihood function by our
  prior on theta.
    * We can then find a posterior distribution of `theta` given the
      observations. This is a real probability distribution.
* Dirichlet distribution is the most common prior distribution over a
  multinomial paramater.
    * Dirichlet prior has the property that posterior is also a
      dirichlet for bernoulli experiments.
    * That's because dirichlet basically gives a weight a starting
      weight to each variable. TODO2: clarify this.
    * Basically, Dir(a_0, ..., a_k) and observe `(M_0, ..., M_k)`
      counts in each, then posterior is `Dir(a_0 + M_0, ..., a_k +
      M_0)`.
    * When the posterior and prior have the same form, the prior is
      called a *conjugate prior*.
    * This is convenient, because we can continue to have a closed
      form for the posterior as we collect more evidence.
* There is some math on Dirichlet which says that `P(X=x_i|theta)` is
  equal to `a_i/sum_j a_j`, where `a_j` are the exponents of the
  Dirichlet distribution.
    * Note that this is the probability of producing `X=x_i` given the
      prior distribution over all values of `theta`, *not over a
      single, known theta value*.
* I believe that everything important about the Dirichlet comes from
  the fact that the probability assigned to theta is proportional to
  the likelihood of the pseudocount result observed.
* I think for any good prior, in the limit, the Bayesian should
  asymptotically approach putting all the density around the MLE.
* In a BN, if there are multiple paramater vertices, the data
  separates these. That is, the paramaters are conditionally
  independent given the data.
    * As before, for table priors, we can learn each separately.
    * That is, if `Theta_{Y|X=0}` can be set independent of
      `Theta_{Y|X=1}`, then we can learn these independently.
    * That's kind of weird, though, because shouldn't there be a prior
      over the the entire space? It seems not bayesian for us to
      unlink these.
* Basically, it works just like MLE. You just accumulate counts to
  update the priors.
* I really don't think she has said anything about hidden variables.
* How to choose priors?
    * One possibility. Set some paramaters `Theta_0` as a wild-ass
      guess.
    * Then do forward inference to get `P(x, u|Theta_0)`.
    * Then set `a_{x|u}=a_0 * P(x, u|Theta_0)`. The `a_0` is the
      *equivalent sample size*.
    * You might set all the prior params as uniform. That sounds
      pretty logical.
    * This is called the Bayesian Dirichlet equivalent sometimes.
* She shows a graph that shows that shows an example where Bayesian
  learning converged much better to the true distribution than MLE.
* MLE in MN.
    * Of course, probability is proportional to product of factors,
      modulo normalizing constant.
    * We can see max likelihood as log-linear, sum of the log of the
      factors, minus `log(Z(\theta))`. Notice that *Z is a function of
      theta*. That's because the normalizing constant changes as you
      change theta! That could make it hard to find MLE...
    * Indeed, there is no closed form solution MLE.
* A little math:
    * Consider log linear model: `P(x)=exp(Sum_i Theta_i f_i(x))`.
    * The derivative of the log of the partition function wrt to
      `Theta_i` is the expected value of `f_i` on the data, where
      `f_i` is the `i`th feature.
    * Second derivatives are the covariance (i.e., the Hessian).
* Okay, here's the idea.
    * Log-partition function is convex (she doesn't really prove it;
      prolly because Hessian is symmetric?).
    * Log-Likelihood is `(Sum_m Sum_i Theta_i f_i(x[m])) - Z(Theta)`.
    * So, as a function of `Theta`, the first term is linear, while
      the second is concave.
    * That means that this is concave. Which means there is a single
      global optimum and should be easy to use hill climbing against.
* Another thought:
    * If you differentiate wrt to `Theta_i`, you're getting the
      difference etween the empirical mean of `f_i(X)` and the
      expectation of `f_i` relative to the implied distribution
      `P_Theta`.
    * Because this needs to be zero, you know that these two things
      should be equal.
    * Which makes total sense. That's what MLE basically is; set the
      parameters such that the empirical expectation is exactly what
      you observed.
* Okay, cool. Optimization isn't insanely hard. But the problem is
  that you have to calculate the partition function at every step. And
  that could be *very slow*.
    * Well, you have to calculate the gradient. But each partial
      amounts to calculate the difference between the empirical mean
      of the features and the expectation of the factors given the
      parameterization.
    * Basically, you have to accumulate the expected feature
      counts. That's the slow part.
    * She notes one positive:
* Conditional Random Fields
    * Reminder: CRF just doesn't have any factors between the input
      variables `X`; you don't try to model the input. You just want
      to model the output variables; `Y`.
    * If you're doing generative training, you need reasonable beliefs
      about how the `X` variables inter-relate, which is really hard
      for us to know.
    * OTOH, with small amounts of data, you can inject your knowledge
      (that is, *bias*) into how variables interact, so you don't
      overfit. But this bias can start to hold you back when you have
      more data.
    * In a discriminative model, you can incorporate wild-ass features
      in.
    * Discriminative models cope poorly with missing data.
* Training CRFs

```
log(L(Theta : x[m], y[m])) = \Sum_i theta_i f_i(x[m], y[m])
                               - log(Z_x[m](\Theta)
```

* What this says, is that we want to choose theta such that we
  maximize the probability of `P_\Theta(y[m] | x[m])`. That's why the
  partition function only takes into account the `x[m]` and not the
  `y[m]`.
* Taking the derivative, we have:

```
d/d\Theta_i l(Theta : D) = 1/M \Sum_{M} f_i(x[m], y[m]) - E_\Theta(f_i(x[m], Y))
```

* Note: the expectation is done over the `Y` values, not the `X`
  values, since we're taking `x[m]` as given and just want to compute
  a conditional distribution on `Y`.
* TODO2: prove the derivative of the partition function is this
  expectation.
* Example for image segmentation:
    * Start with a super-pixeled image (that is your input). The
      output is the segmentation. The segmentation is going to be
      labels for superpixels; e.g., cow and grass.
    * Training from segmented images.
    * Have a feature which multiplies the indicator that a superpixel
      is grass by the intensity of the green in that super pixel.
    * Have another feature which is the indicator of two superpixels
      being assigned the same class.
    * Then for an example, derivative of likelihood wrt the first
      feature type is sum of all green intensities in superpixels
      labeled grass, minus the sum of the model probability of all
      pixels being grass times their greenness.
        * The second term basically says: greenness is a good
          predictor if those things you think are grass in the model
          tend to be green, and the things you don't tend to be
          not-green.
        * In other words, you increase value of the feature when you
          could improve your determintion of what is grass by using
          this greenness feature.
    * Likewise the second feature type. The derivative wrt this
      feature is the observed affinity between superpixels in the
      data, minus the probability that adjacent pixels are the same in
      the model.
        * Basically, you're saying that you should increase the effect
          of the feature until the affinity power in the model is
          equal to that in the data.
* A bad thing about CRFs: since we need `E_\Theta(f_i(x[m], Y))` for
  each `x[m]`, this requires we do inference for each `x[m]`. This is
  *worse* than MRF. This requires `m` inferences at each gradient
  step.
* The good thing is that we don't need to do inference for the joint
  probability, only inference for conditional probability. This can
  eliminate many variables. Also, of course, the model is much
  simpler.
    * I think she also claims that inference is simpler because in the
      MRF, the distributions for the `X` would be very difficult to
      work with. That they may be of a form that makes inference
      difficult.
* Basically:
    * More inference steps required.
    * But inference is typically much simpler (fewer variables,
      simpler distributions).
* We can do a Bayesian type thing by doing MAP for parameters. The
  prior on the parameters is often a Gaussian with zero mean.
    * Also Laplacian distribution is common?
    * What we're going to see is doing MAP in this context is a lot
      like regularization.
    * You can see this by taking the log of
      `P(D|\Theta)P(\Theta)`. This is the log-likelihood plus the log
      of the prior. Which is basically regularization.
    * Log `P(\Theta)` is quadratic for gaussians, linear for
      Laplacians. That corresponds to L2 and L1 regularization.
* She makes a further note. Because L2 reduces pressure to regularize
  a parameter as it approaches zero, we often end up with a dense set
  of weights; few are zero. Whereas L1 has a constant marginal
  penalty, so it results in sparse weights.
* The upshot: it's too hard to do Bayesian estimation in MRF/CRF, but
  we can do MAP to avoid the pitfalls of MLE.

**From book**

* Relative entropy is often used to measure performance of learning:
  prob of learned vs underlying probability.
    * But you don't have underlying probability.
* Definition is `D(P*|P\tilde) = E_{x~P*} log(P*(x)/P\tilde(x))`.
    * So this is equal to `H_{P*}(X) - E_{x~P*} log(P\tilde(x))`.
    * So when comparing two models, we can ignore the first part,
      which is a function of the underlying distribution.
    * So we need only compare based on the second terms. Note that
      this does not give an absolute measure of performance, it just
      allows us to pick between two possible models.
    * Taking `P*` to be equal to the empirical distribution, this
      implies the best solution is the maximum likelihood estimate.
* Of course, we can also measure based on performance on one task.
    * Loss functions might be zero/one loss.
    * Or you might use Hamming loss; which assigns error proportional
      to the number of mispredicted variables; this is useful when
      you're trying to predict multiple variables.
    * Last, you might give error proportional to your *confidence* in
      the estimate.
* Discriminative training is typically used for undirected models
  (called Conditional Random Fields). This makes sense, because to
  discriminative train a bayesian net kind of ruins the interpretation
  of the CPDs.
* Generative models typically have more assumptions and thus higher
  bias. This is helpful with limited data, since the structure helps
  regularize and generalize. But with larger amounts of data, this
  bias can get in the way of using the additional data.
    * In particular, you may have highly correlated features. This
      happens a lot in fields like computer vision, where you might
      put in edge detection, the value of filters, etc. It can be hard
      to have a generative model that accounts for such features.
* Three typical kinds of problems:
    * Paramater estimation
    * Structure learning
    * Learning latent variables.
        * Sounds like this is harder than I think??
* Two kinds of decomposition:
    * Global decomposition: you can estimate each CPD separately from
      every other. At least, this is true when there are no unobserved
      variables.
    * Then you need only the parent values and the variable's value to
      chose the MLE CPD. If it is a table CPD, this further decomposes
      into a series of multinomial distributions.
    * But it's fine if you have a linear gaussian or somesuch.
    * Note: this works with MLE or Bayesian approaches.
* Conjugate priors mean that with samples, you get another of the
  prior family. There is typically a notion of *pseudo-counts*. You're
  just changing the pseudo-counts.
    * I think you want that the expected value of the posterior is
      equal to the MLE of an experiment with the appropriate
      pseudo-counts.
    * The probability of a next experiment's results should be given
      by the MLE of the experiment given the pseudo-counts.
* Sometimes, there is no closed-form conjugate of the CPD
  (noisy-or). Sometimes the conjugate doesn't seem to be what we
  want. In those cases, we might do MAP estimation of the
  paramaters. This is basically MLE, but with some "regularization"
  over the likelihood function.
    * This is often reasonable, because with large amounts of data,
      the posterior tends to be sharply peaked around a single value.
    * The book notes that we have to be careful. MLE and Bayesian
      estimation are independent of the *representation* (AKA,
      *parameterization*) of the prior.
    * But if we reparamaterize, say by stretching part of the line,
      but compressing another, we can fuck with the density in certain
      spots.
    * That can make the max posterior density spot look really good,
      but it may not correspond to the MLE nor the expectation.
    * Prolly the expectation of Bayesian posterior is equal to the MLE
      in the limit of data. But the posterior need not be symmetric,
      so the max density spot does not have to be the expectation.
* Learning all CPDs separately will require lots of data.
    * A lot of times, different variables share the same CPD.
    * So we can aggregate sufficient statistics.
    * This requires a strong structural assumption. If wrong, it can
      hurt performance. It might still be good when there is little
      data.
    * This is called *global parameter sharing*. That's because
      parameters are shared between different CPDs.
* May also have *local parameter sharing*. This happens at a CPD.
    * We've been expecting that there is a local decomposition for
      tabular CPDs.
    * This is common for tree-based CPDs. If you think about the
      corresponding tabular CPD, then several entries are the
      same. Thus you aggregate over these cases.
* A careful note about global parameter sharing:
    * For a single variable, you can compute the posterior probability
      of a sample by taking `\Int_{\Theta}
      P(D[X+1]|\Theta)P(\Theta|D)d\theta`.
    * In a BN, you find the posterior distribution by doing this at
      each node and multiplying.
    * However, if there is global sharing, this is not quite
      right. Really, if two variables share parameters, we should use
      `\Int_{\Theta} P(D_i[X+1]|\Theta)P(D_j[X+1])P(\Theta|D)d\Theta`.
    * Basically, we are trying not to repeat the `P(\Theta|D)` twice.
    * This doesn't happen for local sharing, BTW.
    * Computationally, this is annoying.
    * With a large number of training examples, we become pretty
      confident of the parameters, so this should be less of a
      problem?
* Hierarchical Bayesian Model
    * Basically, say you didn't just want to *share* parameters. You
      wanted to do something like LDA where parameters themselves have
      priors.
    * This approach is when we have small amount of data, but think
      that parameters are related, but not necessarily identical.
    * We haven't discussed how to learn models like this, because now
      there is effectively latent variables.

**From Homework**

* There's a note that when training a CRF, you have to calculate model
  feature counts. That means you have to consider `Sum_Y P(Y|x)f_i(Y,
  x)`. If `Y` consists of a lot of variables, you must collect many
  counts.
    * But if `Y` is only pairwise, then you have things pretty
      easy. You only have to do this for four settings of `Y`.
* Also they mention: computing gradient for a datapoint is very
  slow. That's one big reason why SGD is really helpful here.

## Week 8: Structure Learning

* Two kinds of errors: missing arcs or extra arcs.
    * Missing arcs means you can't learn the model properly.
    * Additional arcs can theoretically be learned to be useless, but
      they're at risk of causing overfitting and worse generalization.
    * Not always clear which will give better performance.
* Learning of structure is score-based.
    * One approach is to use MLE and search over G *and* theta
      simultaneously. That sounds really hard.
    * Say you add one edge from `X` to `Y` to model `G1` producing
      model `G2`. She shows that the MLE benefit is: `M*I_P\hat(X;
      Y)`, which is the mutual information of and `X` and `Y` in the
      empirical distribution.
    * Basically, the increase in log likelihood performance is
      proportional to the number of bits you'd save by adding this
      feature.
    * NB: mutual information ranges from 0 (consider to indep vars) to
      `\inf` (consider to deterministic vars; depends on entropy of
      one of them.
    * For binary vars, max mutual information is `+1`.
* More generally, log likelihood can be seen as the sum of the mutual
  informations of `X_i` given its parents in the graph, minus the
  entropy of the `X_i`.
    * Basically that's just applying log to the likelihood
      function and interpreting the terms as bits.
* This is telling us that we want to add edges when then parents are
  correlated with the children. It's also probably making it easy to
  consider greedy approaches to growing the network.
    * Of course, it's hard to observe perfect mutual independence in
      the empirical data. That's actually very unlikely.
    * So this criterion would tell you to put all the edges in, which
      we know is wrong.
    * That's basically like saying: adding a parameter can never hurt
      a model, so give me all the parameters to optimize with.
    * Of course generalization will be poor.
* Ways to avoid are:
    * Restrict hypothesis space: # of parents or # of parameters.
    * Or we can penalize complexity explicitly. Something tells me
      this is BIC.
* BIC takes log-likelihood and subtracts `(log(M) / 2)Dim(G)`, where
  `M` is the number of examples, and `Dim(G)` is the number of
  independent parameters.
    * Why this comes later. But clearly trades off log-likelihood of
      data with model complexity.
    * The negation of BIC is (approximately?) equal to MDL she says.
    * But the idea is that BIC/MDL can be justified from a standpoint
      of Bayesian statistics, or equivalently from an information
      theory standpoint.
    * If you break down BIC, you have:

```
M\Sum I_P\hat(X_i; Pa_{X_i}) - M\Sum H_{P\hat}(X_i) - (log(M)/2)Dim(G)
```

* Note that the middle term is constant. Also, note as `M\to\infty`,
  first term will dominate second term. So model complexity can grow,
  and preference for more complicated models that fit the data better.
* In fact, she claims that there is *consistency*, that as the amount
  of data grows, the score is maximized by the true structure of `G`.
    * That surprises, why no spurious edges?
    * Prolly because as data grows, desire for spurious edges goes
      down, and can be dominated by the penalty, which is slow
      growing?
    * But wouldn't that be true even if we didn't have the `log(M)/2`
      factor? The value of spurious edges would eventually fall below
      the value of additional model complexity, right?
    * I guess we need some factor of `log(M)`, because even though
      value of spurious edges goes down, there's more samples, so the
      number of bits saved may be constant...
    * She's hand-waving here, that's okay, I'll look to the book.
* Another approach is Bayesian scores. Perhaps we want the MAP of
  `P(G|D)=P(D|G)P(G)/P(D)`. Of course, `P(D)` is a normalizing
  constant that we can ignore.
    * Won't it be a pain to calculate `P(D|G)`; we need to learn the
      params, right? But possible.
    * Curious what the prior `P(G)` would be...
    * Often see this as a log probability, where it's a sum.
* `P(D|G)=\Int P(D|G, \Theta_G)P(\Theta_G|G)d\Theta_G`.
    * The second part of the product is like a prior for parameters.
    * She claims that this term here will reduce the desire to overfit.
    * She says that's because, whereas in MLE we get to choose the
      `Theta\hat` that optimizes likelihood for `G`, here we have to
      consider all possible parameter settings.
    * That tends to reduce the benefit.
    * So she says that this is probably the more important effect than
      the prior. I think that this term will still favor more
      complicated models, but will reduce the benefit of a more
      complicated model.
    * She goes to show that for multinomials with dirichlet priors,
      you can calculate `P(D|G)` with a closed form. I don't think I
      care at this time about the actual closed form...
* Actually, I think I'm wrong. I think the marginal likelihood
  `P(D|G)` *does* impose a penalty on complexity. That's why people
  often use *uniform priors on graph structures*. Whoa.
    * They often make `P(G)` proportional to `c^{|E|}` (c between zero
      and one).
    * Normally we would have to normalize across *all possible graphs*.
    * But that is not necessary, since the normalizing term is ignored
      anyway.
    * Once again, we use BDe prior for `P(\Theta_G|G)`.
    * Reminder: we set some structure `B_0` over the graph. This is
      typically the fully disconnected graph. We then compute `P(x_i, pa_i|B_0)`.
    * We then take this, multiply this by the *equivalent sample
      size*. This tells us the BDe-prior for `P(x_i|pa_i)`.
    * Apparently there's some proof that the BDe prior is the only one
      where two I-equivalent must have the same Bayesian score. She
      doesn't prove it. It would seem stupid to prefer one of two
      equivalent networks.
* She makes a note. That if `G` is a network with Dirichlet priors,
  then `log P(D|G)=l(\Theta\hat_G: D) - (log(M)/2)Dim(G) + O(1)`, as
  we take `M\to\infty`.
    * This is BIC plus something that doesn't change given `M`.
    * Which is saying that BIC is equivalent to `P(D|G)` **in the
      limit**.
    * Which also means that Bayesian score is consistent since BIC is
      consistent. But that kind of alrady makes total sense given
      where the Bayesian score is coming from.
    * Claim: BIC tends to underfit when M is small.
    * Prolly a tradeoff: BIC can probably work in networks which don't
      use multinomial/dirichlet priors. Prolly much harder to do that
      kind of thing for a Bayes score...
* Learning Forest of Trees
    * Efficient, sparse parameterization.
    * Sparse parameterization is also nice because less suceptible to
      overfitting, have less noisy estimates.
* Because of decomposition, setting `p(i)=parent idx of i`, and zero
  if root:

```
score(G:D)=\Sum_{p(i)>0} score(X_i|X_{p(i)} : D) + \Sum_{p(i)=0} score(X_i : D)
```

This is equal to:

```
\Sum_{p(i)>0} score(X_i|X_{p(i)} : D) - score(X_i : D) + \Sum_i score(X_i : D)
```

And the first term suffices because second is constant. And basically
the idea is that we include an arc if the score with the edge is
better than the score without the edge. That makes total sense. It's a
greedy, prolly parallelizable approach.

Basically what you do is you find all these weights, and then use a
standard graph theory algorithm to find a max-weight spanning
tree. Your graph may be disconnected if you only include positive-edge
costs, so you do this on connected components. Note: you run this in
an undirected graph; you're helped by the fact that the score for an
edge is symmetric; the same in one direction as the other. That's a
property of mutual information.

* More general structures: it's NP hard to find max scoring network
  when vertices can have a max of `k` parents, whenever
  `k>1`. Whoa. That sucks.
* Standard approach is heuristic search. So we make local
  preturbations.
    * You choose how to generate next candidates. Local steps add,
      delete, or reverse edges. You can also have global steps, a la
      Metropolis-Hastings.
    * Lots of search techniques. Greedy hill-climbing, best-first
      exploration, simulated annealing...
* To start, we might use:
    * Empty network. Perhaps favors sparsity.
    * Best tree. This was an optimum in a constrained problem, so
      maybe a good starting point.
    * Random network. This forces us to explore the space more fully.
    * Expert opinion of structure. Benefit from expert, prior
      knowledge.
* Local maximum are an obvious problem. But also plateau, because many
  equivalent networks will be neighbors.
* Edge reversal is important because operation of remove and replace
  in the opposite direction may not satisfy greedy nature of search.
* To avoid local maxima/plateaux, we:
    * Random restarts. Rather than totally restart, we can take a
      number of random steps whenever stuck.
    * Tabu list: keep a number of recent steps and don't allow them to
      be reversed.
    * Basically, techniques of combinatorial optimization from Norvig
      apply.
* Koller claims that learning structure *and* parameters actually
  isn't that bad. It doesn't require many more steps to get a similar
  performance, as measured by the KL-divergence of `P` and `P\hat`.
    * That is, your structure might be wrong, but it is mostly
      generating the correct joint prob distribution.
* Interesting note: Koller gives some examples of structure being
  learned succesfully.
    * In one example, edge is directed wrong way. But reversed edge is
      I-equivalent.
    * There's no way to learn that difference, even in the limit of
      data.
    * That's because this requires a causal model.
* Whoa, she says there are much better algorithms. They're the result
  of *global steps in the space*. But fuck, is she not going to teach
  us? Fuck you!
* Time complexity of search:
    * `O(n**2)` possible improvements.
    * Evaluate improvement: `O(n*M)`, because there are `n` vertices,
      and you have to look at scores at each of those points.
        * I would think there would be a lot of ability to share
          work. Most improvements are perturbations which just perturb
          the score.
        * Indeed, she notes this because of decomposability.
        * I think it should be `O(1)` to see the change in the score.
        * Oh yeah, you also get to save work from previous steps!
          Most calculated deltas for a change typically don't change
          step-by-step.
    * Should also be checking to be acyclic. Typically proportional to
      number of edges in the graph.
* All told, it should take `O(nM)` per step. Because you only need to
  update `O(n)` delta-scores after selecting a change, and that
  requires computing statistics, which needs to look through all the
  data, thus `O(M)` per delta-score.
* Of course use a priority queue for selecting next change.
* Further:
    * Typically there is only a small subset of possible parents for a
      vertex. If we can find them, then we can greatly speedup this
      process.

## Week 9: Learning with Incomplete Data

* Happens with hidden variables, or missing values.
* Why do you want latent variables?
    * Often sparser decomposition.
    * Also, latent variables may be interesting. They may give
      structure or form clusters of data. This could be useful, e.g.,
      for other processes.
* How to add observability into model?
    * Could create `O_i` binary nodes for whether a variable is
      observed.
    * Then create a `Y_i` for every `X_i`; this is deterministically
      the value of `X_i` if parent `O_i` is true (observed), or `?`
      if `O_i` is false (unobserved).
    * This allows you to handle scenarios where observability is not
      independent of the other variables.
    * *Missing at Random* means you can ignore mechanism for
      observability. So that's simpler.
    * Note that in medical diagnosis, we don't have missing at random
      as an assumption. E.g., if a MD doesn't give a chest X-ray, then
      that's an indication patient doesn't have TB.
* With latent variables, multiple global maxima for likelihood.
    * E.g., for each binary variable, you could swap `1` for `0` and
      that's fine. The values don't mean anything to you.
    * Apparently this can happen in hidden data.
    * Does that really matter? Aren't all the global maxima symmetric?
* Likelihood for incomplete data is basically sum of likelihoods of
  each possible completion of the data.
    * That can be *multimodal*.
* Another problem: parameters become correlated, and cannot be learned
  independently.
    * For instance if you have a model `X->Y`, with parameters
      `Theta_X` and `Theta_{Y|X}`, but don't observe `X`...
    * Now different settings of `Theta_X` would push you toward
      different settings of `Theta_{Y|X}`...
    * In the graphical model, the observation of `Y` creates a
      *v-structure* between the two (parameter) variables!
* But I might argue: if you don't ever observe `X`, who cares; all
  these possibilities are equivalent in the observable world.
    * The difference is when you *sometimes* see `X`. Then missing
      data can cause correlation between the parameters.
    * It's not as extreme as when no data is observed, but it is a
      real factor.
    * Koller showed some graphs which demonstrated how this
      correlation emerges even in the presence of just a few
      datapoints.
    * Might note: that doesn't mean there are multiple optima, but it
      does make our learning of parameters more difficult.
* Expectation Maximization
    * Look, your `L(D|\Theta)` is going to be nasty and multimodal
      because of the unobserved data.
    * In particular, it won't have a nice closed form. So it's not
      going to be possible to solve exactly.
    * So we could do gradient ascent.
    * It's not hard to calcualate partial of log likelihood wrt a
      parameter. For `Theta_{x_i|u_i}`:

```
log P(D|Theta) = Sum_{x_j, u_j} Sum P(x_j, u_j | d[m], Theta)P(Theta)
dlog P(D|Theta)/dTheta = 1/Theta Sum P(x_i, u_i| d[m], Theta)
```

* Problem: to do this you need to know probs on every `u_i` for each
  `d[m]`. Which means you need to run inference for every data
  point. And you need to do this for every step of the gradient
  process.
    * Also need to be careful, we're supposed to be doing a
      *constrained* optimization problem (need to have properly
      normalized CPDs). So that's an additional complication, but not
      impossible.
* Expectation-Maximization:
    * Initialize parameters randomly.
    * Do a *soft-completion*; find prob distribution over missing
      vars.
    * Find the *expected sufficient statistics*; basically, use the
      soft-completion and pick the max-likelihood parameters for the
      completed data.
* Need to run inference for each data instance at each iteration.
    * That's the same as gradient ascent.
    * But EM is very simple to implement.
    * But the pro is that EM makes rapid progress, especially in early
      iterations.
    * Koller mentions that EM can converge faster than gradient
      descent in early iterations, but sometimes for later iterations
      conjugate gradient descent is used, since EM stops making rapid
      progress.
* Thoughts about EM
    * After the soft completiong, we have a new likelihood function
      wrt the completed data.
    * EM jumps to the maximum of the likelihood function.
    * Basically, EM is approximating the likelihood function wrt the
      missing data by doing the soft completiong.
    * This can contain more information than gradient ascent's linear
      approximation.
    * She does a mathematical proof. Basically, given the incomplete
      data and the parameters, using the expected statistics basically
      gives us the expected likelihood of the `theta'` given the
      distribution of completions of the data wrt `theta`.
    * Okay, great.
* Results:
    * EM improves on each iteration.
    * When EM converges, it has found a stationary point of the actual
      likelihood function. That makes sense.
* EM in practice
    * Convergence in likelihood space may preceed convergence in the
      parameter space. She shows graphs where the likelihood has
      converged, but the parameters are fluctuating wildly. That's
      weird.
    * It looks like what happened was that EM started strongly
      overfitting. Basically, parameters were going wild, but they
      weren't improving the result by very much.
    * You might want to just evaluate validation set data at each
      iteration and stop when the validation perf starts to go down.
    * Another possibility is to do MAP instead of MLE, which might
      prevent some overfitting.
    * She mentions this also happens with gradient ascent, so it's not
      actually EM specific.
* Local Optima
    * With missing data, you have local optima. She shows that even
      with 25% missing data, there will be many local optima, and they
      go away slowly with number of trianing instances. With truly
      hidden variables, they never go away.
    * But why do we care? Does it matter if there are many local
      optima?
    * Koller shows a graph showing that there can be huge variation in
      the likelihood of various local optima. That is: some local
      optima are much better than others. It's not like all optima are
      created equally.
    * Simplest approach is random restarts.
    * We could also try to start from a more sensible starting
      point. We could use prior knowledge, or the output of some
      simpler process.
    * She says: *initialization is critical*.
* Learning with Latent Variables
    * Common to do Bayesian clustering.
    * HMM for speech recognition.
        * Often has two HMMs, one for phones, one for words.
        * She mentions that they often train a baseline, to use as an
          initialization for the phone model.
        * Then they train the segmentation of words. They retrain the
          phone models too, but the initialization helps them find a
          good local optimum.
    * 3D Robot Mapping
        * Observations: laser rangefinder readings.
        * Parameters: location of walls.
        * Latent Vars: assignment of points to walls.
* How to choose how many latent variables
    * BIC (tends to underfit), or BDe extensions.
    * Metrics of cluster coherence (if doing clustering...).
    * Or you can use Bayesian techniques which average over different
      cardinalities.

## Wrapup

* PGMs shine when incorporating domain knowledge.
* Also when we want to reason about multiple variables. If you're just
  trying to predict one variable, may not be necessary to use PGM.
* Also good when producing rich models form basic building blocks.

## Random Baum Welch Note

* It's interesting that when computing the transition matrix, you give
  equal weight to the first transition, which you have the greatest
  confidence in, and the last. That's funny because as the markov
  chain mixes, you have less confidence in the probability of future
  states.
    * Do you? Who says your initial probabilities are certain? And
      don't you get a lot of data as you go indicating what state you
      may be in?
    * Might be interesting to have a prior. That way transitions
      you're less certain of contribute less to the newly computed
      tranisition matrix than transitions you are not confident in...
