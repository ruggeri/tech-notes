# Ch1: Introduction

* Big Names
    * Minsky, Newell and Simon, McCarthy, Pearl
* Big ideas
    * Search, Neurons
    * Genetic programming
    * Backpropagation
    * HMMs
* Approaches:
    * Think humanly/rationally
    * Act humanly/rationally

# Ch2: Intelligent Agents

* Dimensions:
    * Observability: Full or partial
    * Number of Agents: Single or Multiple
        * Also, competitive?
    * Deterministic or Stochastic
    * Sequential or Episodic
        * As in, are there a sequence of decisions to make
        * An episodic task might be image-recognition, each image is
          independent of prior recognized images.
    * Static or Dynamic
        * Does the environment change while the agent makes a decision
    * Discrete or Continuous
        * What is the cardinality of the decision the agent makes.
    * Known vs Unknown: does the agent know what the effect of its
      actions will be. Does it know "the rules of the game"?
* Most agents try to maximize some utility function.
    * May need to use expectation in stochastic environments.

# Ch3: Solving Problems By Searching

* BFS and DFS are simple approaches.
    * Both have `O(b**d)` time complexity (where `b` is depth to a
      goal node)
    * But DFS has `O(bd)` memory usage.
* May have to keep track of previously visited nodes to avoid
  repetition. That doesn't add memory for BFS.
    * For DFS, maybe just try to keep out of cycles by looking back at
      your path. You can't really afford to keep track of where you've
      been.
* Iterative deepening
    * Avoids going down a wrong path, so answer found is optimal.
* Dijkstra's algorithm
    * For problems where step cost is not constant.
    * This is sometimes called *uniform cost search* in AI literature.
* Backtracking search
    * Really the same as DFS, just a memory optimization.
    * Memory optimization of DFS if you can generate successor moves
      one at a time.
    * `O(d)` memory usage.
* Bidirectional search
    * Search from both sides. Should have `O(b**d/2)` time complexity.
    * But you need to keep a fringe on at least one side, so memory is
      `O(b**d/2)` as well.
* To do better, we need to use *heuristics*.
* A-star is complete/optimal if you use a *consistent* heuristic
    * A consistent heuristic is one where paths only prove to be worse
      than expected as you explore them.
    * `h(n) <= c(n, n') + h(n')` for `n'` a successor of `n`. The idea
      is that the estimate only gets worse as you continue.
    * This is stronger than *admissability*, which just says you never
      overestimate.
    * Consistency means you explore a node until you know you've got
      the best path to it.
    * Admissability is okay in trees, since there aren't two paths to
      any node.
* A-star is optimally efficient for a given heuristic.
    * Does just enough work to know there isn't a shortcut to the goal
      node.
* IDA-star
    * The difference is that instead of increasing the depth by one
      each time, the next iteration uses the cost smallest pruned
      path. This only explores *a little bit more* each iteration.
    * Notice that IDA-star uses *too little* memory; it has `O(1)`
      memory usage over DFS, but there could be more to take advantage
      of.
* SMA-star
    * Starts exploring the tree, keeping the full tree in memory,
      always expands the node with the lowest total (path + heuristic)
      cost.
    * However, to expand a node, it may have to prune a node to
      maintain its memory bound. It picks the leaf with greatest cost.
    * It then stores this value in the parent. The parent will not be
      re-explored until all paths of lesser cost have been explored.
* If heuristic `h1` dominates `h2`, then `h1` will lead to better
  performance.
* Can often formulate heuristics in terms of simplified subproblems:
    * In the 9-puzzle, how many squares are in the wrong position?
    * Better: what is the sum of taxicab distances to the right
      position.
    * This is clearly admissable. Prove that it is consistent.
* Max of several heuristics is fine. Overlapping heuristics are
  difficult because they may not be disjoint.
* Also can have a database of subproblems.
    * E.g., 8-puzzle, solve it where you only have to lock in 4 of the
      squares. Build a database with the cost for each of the
      positions mof the four. Can then use this as a heuristic.
    * Apparently very successful.
    * But combining heuristics can be hard. Can always take the max.
    * But can formulate in disjoint ways. E.g., in 8-puzzle can just
      count the number of moves using only those tiles that need to be
      locked in.
* Question: can we *learn* heuristics as we go? What features might
  suggest themselves? This is mentioned but not explored in the text.
* Question: what about if we are satisfied with a "good enough"
  solution?

**TODO2**: Implement SMA-star.
**TODO2**: Implement disjoint heuristics for 8-puzzle.

# Ch4: Beyond Classical Search

* Local search explore interactively. Can use hill climbing.
* The biggest problem is falling into local optima.
    * Can try stoachstic hill climbing to choose from uphill moves.
* Random-restart: choose a starting location randomly and repeat.
    * In eight queens, start the queens out in random configuration
      and try to move from there.
* Simulated annealing
    * Generate a random move each turn, accept if better.
    * Else accept with a given probability.
    * Slowly reduce that probability (cool down).
    * Idea is to "shake" out of local minima.
* Local beam search
    * Start in k random positions, generate successors.
    * Pick the k best successors. Note that this is *not*
      random-restarts. The k chosen successors may all come from the
      same first location.
    * To avoid immediately clustering, stochastic beam search chooses
      successors randomly, with probability a function of the value of
      the successor.
* Genetic algorithms
    * There's a fitness function.
    * Crossing over happens between pairs chosen by fitness.
    * Point mutations also happen.
    * Really only works *if the schema makes sense*.
    * Note how this contrasts from real genetics. In real genetics you
      can relocate mutually advantageous stuff together and it will
      still work. You don't get this with an arbitrary schema.
* Continuous spaces
    * Can do Newton-Raphson. But that requires finding the inverse of
      the Hessian.
    * Can do line search with just the gradient. A trouble is the step
      size.
    * Common approach is to double step size until fitness falls, then
      reset to small step size.
* Special case: Linear programming. Can use Danzig's algorithm.
* Non-Deterministic Actions:
    * If discrete actions lead to discrete states, can do "AND-OR
      search" (which is basically minimax).
    * Have to handle potential loops. Kind of a pain. Note that
      sometimes you are in a loop that if you keep trying you'll break
      out of (with probability 1). That happens, for instance, if an
      action fails and you stay in the same state.
* Partial Observability
    * Can still reason with no sensors.
    * You have a *belief state* which is all possible states you could
      be in.
    * Each action takes this set to another set. If actions are
      deterministic, should only shrink.
    * If you do have some observability, then you can, after doing an
      action, use that observation to "break up" the new belief state
      into subsets of possibilities.
    * Basically is and-or search from before; your program needs to
      work no matter what of the current states you might be in.
    * Is tougher when you throw in non-determinism. Then you have that
      loops problem again.
* Partial Observability and Unknown Environments
    * We may not even know the state space.
    * Have to solve the problem *online*; can't pre-compute ahead of
      time.
        * Online is also good in nondeterministic environments because
          you only compute based on those eventualities *that actually
          happen*.
    * May have a heuristic on distance to goal.
    * *Competitive ratio* is the ratio between best possible path, vs
      path found without knowledge.
    * Could try hill climbing, or random walks.
    * Regular A-star would find the best path, but be inefficient to
      return to the root each time.
    * Learning real-time A-star (LRTA-star) chooses the apparently
      best choice from its next location. But it also updates previous
      location if our estimate was overly optimistic.

**TODO2**

* 8-queens problem. What other good problems?
* Implement local beam search, simulated annealing.
* Implement line search. Multidimensional Newton-Raphson.
* Implement a partially observable problem.
* Implement LRTA-star.

# Ch5: Adversarial Search

* Minimax algorithm.
    * Basically AND-OR search.
* You can improve with alpha-beta pruning. Effectively allows you to
  double the number of plies (best case).
    * *Move ordering* is important.
    * Consider if you always examine maximizer's best move.
    * As you consider the other moves maximizer makes, consider the
      minimizer's best response.
    * This leads you to a worse outcome. So you can throw away this
      maximizer's alternative quickly.
    * This means you basically can "ignore" all maximizer levels from
      analysis. That gives means you can go twice as many plies deep.
* Unordered, you acheive an expected `O(b**(3d/4))`.
* Of course, you do have to go all the way to the bottom. You can use
  heuristic evaluation after a number of levels deep.
* Iterative deepening is useful in case you get interrupted and forced
  to produce best known move. Also helps with move ordering.
    * Basically, you record the *killer move*, the best move at a
      position from the previous level of search. This can be a lot
      better than heuristics.
* You can also store a *transposition table*, which is previous
  evaluations of nodes, so that repeated states can just look up their
  value.
* Evaluation can be done by featurization of previous games; each
  feature can be weighted to create an aggregate prediction of the
  likelihood of winning.
* You usually look for *quiescent positions* to stop at; you don't
  want to stop at a point where there are dramatic possible changes in
  the near future. That would be shortsighted. Your *cutoff function*
  should typically more complex than just pure depth.
    * In particular, you might just consider capture moves.
* Still may suffer from *horizon effect* where the inevitable is
  delayed.
    * One solution is to note "singular extensions". These are moves
      that are "clearly better" than any other move. When you hit the
      depth limit, you ask whether the singular move applies. You
      search a little deeper, but it isn't that much, and you'll see
      that certain things are inevitable.
    * Meh, I'm not that motivated by this one.
* Forward pruning
    * Beam search is too aggresive.
    * We don't normally evaluate deeply moves that look bad.
    * ProbCut is a probabilistic version of alpha-beta
      search. Alpha-beta prunes only those branches *provably* outside
      the `(alpha, beta)` window. ProbCut instead uses statistics of
      previous games to prune those branches which are very likely to
      be outside the range.
* Table Lookup
    * First, can gain efficiency via transposition tables to see if
      the state is effectively the same as another state.
    * For opening game, typical to mimic approaches extracted from
      study of many past games.
    * For closing, can offline compute a policy by running *reverse
      minimax* on all endgames. This is kinda like search from both
      sides.
* Stoachstic games
    * Use minimax, plus try to maximize expected value.
    * This is harder because you can't do pruning.
    * Note that if using a heuristic must be linear in outcome value
      for expectation to still make sense.
        * Otherwise you'll distort the expectations.
        * Your heuristic, e.g., ought not be `value**2`.
        * This is a tougher requirement than for minimax.
    * Typically don't look deep because any individual future is
      unlikely anyway.
        * One good thing is it means that limited depth may not be
          that bad?
    * Can also do a *rollout*. You play a simple strategy against
      itself. You see what next move appears to win the most
      games. This requires you to play to completion, but the win
      percentage can be quite revealing of the best move. The strategy
      you try can use a really dumb heuristic and it's still useful.
    * Presumably you can do much fewer trials; you won't actually run
      through ever possible game.
    * Card games. The original shuffle is the only randomness. Do
      expected, minimax, but do it on a random sample of shuffles. You
      could do it on all the shuffles, but that's prolly too many.
    * But this is called *averaging over clairvoyance*, because the
      minimax on one of the sample problems assumes full knowledge,
      and we don't know what state we're in. So that won't typically
      work.
* Null Move heuristic
    * Quickly establishes a lower bound on the value of a
      position. Allows the opponent to move twice at the beginning,
      then executes a shallow search.
    * We can then start our alpha-beta with this as our alpha.
* All-in-all, kind of unsatisfying, becaue search is ultimately pretty
  blind and doesn't engage in real reasoning.

**TODO2**

* Implement killer moves for chess. Implement transposition table.
* Implement forward pruning. Null move heuristic?

## Ch6: Constraint Satisfaction Problems

* Can represent system of constraints as a graph with constraints
  being connections between variables. Vertices store domains of the
  variables.
* Can turn multi-variable constraints into binary constraints via the
  addition of new variables.
* Often have preference; than the CSP is a form of optimization
  problem.
* Constraint propagation
    * Vertex consistency is where you've restricted values of domains
      to meet the constraints.
    * Arc consistent if every value in the variable's domain is okay
      with other adjacent neighboring possibilities. For instance, if
      Y must be a square of X, then if we have `X={1, 2, 3}`, `Y={4,
      9, 16}`, we should remove `X=1` and `Y=16` from the
      possibilitis.
    * Can acheive via AC-3; keep a queue of all arcs. Remove an arc,
      and make the two variables arc consistent given the arc. If this
      revises a domain, we must add all the arcs from that vertex to
      the queue so that the restriction is *propagated*.
    * Consider if there are `d` values in each domain. Each of `c`
      arcs can be added to the queue only `d` times. Checking
      consistency of an arc can be done in `O(d**2)` time (considers
      all pairs of possibilities, and whether they obey the
      constraint). Total time is `O(cd**3)`.
    * Can also generalize to n-ary constraints (called *generalized
      arc consistent*).
* Arc consistency can reduce possibilities, but won't typically find a
  solution. We can extend the concept to *path consistency*. Path
  consistency says for any 3 variables, a consistent setting of 2
  variables allows a consistent setting of the third.
    * PC-2 algorithm does this; very similar to AC-3.
    * We can generalize this to *k-consistency*. 1-consistency is
      vertex consistency, 2-consistency is arc consistency, and
      3-consistency is path consistency.
    * *Strongly k-consistent* means that it is also
      `(k-1)`-consistent, `(k-2)`-consistent, etc.
    * It is possible to be `k`-consistent without
      `(k-1)`-consistent. Given any consistent two settings of the
      vars, a possibility for a third var might exist; but given a
      setting of just one var, it a 2nd var might not have a value.
    * If `k=n`, then we can solve by picking a value for the first
      var, then for the next, etc.
    * Takes `O(d(n**2))` time; since at each of `n` steps we need to
      pick one of `d` options consistent with the previous `i`
      choices.
* This is all in terms of binary constraints; we can also have special
  purpose algos for global constraints, like `ALLDIFF`. Another
  example is for large integer sets; if there are bounds constraints
  (LT or GT), we can do *bounds propagation*.
* Backtracking search: chooses a value for each successive
  variable. Either gets to the end, or we have to backup to choose a
  different value.
    * Heuristic for choosing next variable is minimum number of
      remaining values (MRV).
    * Selecting value is often best to choose the least constraining
      value.
* Can do forward checking; when locking in a value, maintain arc
  consistency. If you run out of values, then can immediatly
  abort. Normally backtracking might not realize we made a mistake
  until much later, when we selected the overconstrained variable.
* MAC (*maintaining arc consistency*) is an algorithm which does AC-3
  after every choice, so that arc consistency is always maintained.
    * More powerful than just forward checking.
* Backtracking moves back to retry the most recent decision, but that
  might not fix the real problem. We want to *backjump* to the most
  recent variable that overconstrained the problem variable with the
  empty domain.
    * We could just pop up the stack, but we can also keep *conflict
      sets*; every time we remove a value from the domain, we add that
      variable to the conflict set.
    * But since conflict are immediatly surfaced by forward checking,
      backjumping is unnecessary.
* We can enhance backjumping. Say that we choose `X`, followed by a
  bunch of irrelevant variables `Y_i`. We then choose `Z_1`, which
  overconstrains `Z_2`. We immediately revisit our choice of
  `Z_1`. But say this was the only value of `Z_1` possible due to the
  choice of `X`. We want to backjump all the way to `X`.
* The way to formalize this idea is *conflict directed
  backjumping*. As we jump up, we merge conflict sets. So in the
  example we merge the conflict set of `Z_2` (maybe just `Z_1`) with
  the conflict set of `Z_1` (maybe just `X`). The idea is that there
  is no simultaneous solution of `Z_1` and `Z_2`, so someone amongs
  the union of the constraint sets of `Z_1` and `Z_2` fucked us.
* A *no good* is a minimal setting of variables that *can't work*. The
  constraint set represents a no good, since it caused a contradiction
  at this variable. It is useful to record no goods in a database so
  that these are not revisited, wasting more time and effort. Text
  claims this is important to modern CSP solving performance.
* Local search is another strategy; you randomly set the variables. In
  choosing, you can choose the value which results in the minimum
  number of conflicts.
* You can enhance this with *constraint weighting*; this weights the
  min conflicts heuristic. Each time you pick a variable, you
  increment by one each edge that constrains a neighbor variable. This
  helps you focus on those constraints that are difficult to satisfy.
    * Also *tabu search*, which keeps a memory of recently tried
      solutions so you don't revisit them.
* Local search is good for online problems; you can add or tweak
  constraints to an old problem, and local search for an update to
  your current solution.
* Could consider structure of problems. Finding independent
  subproblems is a big win, but basically never happens. Trees are
  also easy to solve.
* We can solve tree by acheiving *directed arc consistency
  (DAC)*. That's just says a parent's domain is consistent with the
  entire subtree's domains. We can start from leaves and work our way
  up to acheive DAC.
    * If there are `n` vertices, than there are `n-1` edges. Each edge
      requires checking consistency of each parent domain value with
      the allowable child domain values. This means that the time
      complexity to acheive DAC is `O(nd**2)`.
    * We can then solve by working our way down from the root. Pick a
      root value, then filter its children's domains, picking child
      values and so forth. Each selection takes `O(d)` time, so
      `O(nd)` to solve once we have DAC.
* Most graphs aren't trees, but maybe we can reduce them to trees. We
  could try removing nodes.
    * Find a *cycle cutset*; a set of vertices that, when removed
      leave a tree.
    * Assign values to the cutset, reduce domains of tree to be
      consistent with cutset's values. Solve the tree. If this is
      inconsistent, try other values for the cutset.
    * Finding a minimal cycle cutset is NP-hard, but there are
      approximation algorithms that run fast to find small enough
      cutsets.
* Another possibility is to "combine" vertices into subproblems.
    * Every vertex must be in some subproblem.
    * If two variables are connected by a constraint, they must both
      be in subproblem (along with the constraint).
    * If a variable is in two subproblems, than it must appear in
      every subproblem on the path between these two subproblems.
        * Basically, there needs to be a path between them for the
          information to be passed between the subproblems.
        * "Running-intersection" is basically just a restatement of
          that property.
    * Constraints between subproblems are "equality" constraints; that
      variables involved in both subproblems take on the same values.
    * If we decompose into trees of subproblems size at most `w`, then
      it takes `O(d**w)` time to solve root subproblems. We then
      proceed up a level, constraining the variables of the next
      problem (takes `O(d**w)` time). We solve this and keep moving
      up. Thus the overall time complexity is `O(nd**w)`.
    * Of course, finding the best tree decomposition is hard, but
      again can use approximation algorithms.
    * Also: note that graphs with bounded subproblem size are solvable
      in linear time.
* Last, values are often arbitrary. For instance, in graph coloring,
  and permutation of the colors makes no difference to a
  solution. Thus we can break *value symmetries* to reduce unnecessary
  complexity.
* Note: sounds like there is research in hybradization of local search
  and inference techniques. Also distributed constraint satisfaction
  seems interesting.

**TODO2**

* Write AC-3.
* Try out MAC on Sudoku. Use heuristics for variable selection, value
  choice.
* Try out conflict-directed backjumping on Soduku.
* Try to implement detection of no goods
* Solve problems using local search. Maybe sudoku would work?
* Solve tree problems. Try to search for trees.

## Ch13: Quantifying Uncertainty

* They note that agents keep track of belief states as they interact
  with the environment. There can be an extremely large number of
  states to keep track of. Some of them are probably very
  unlikely. And we may not have any action that is good for *all* of
  them.
* So if we consider mostly *likely* states, then we (1) track less
  information and (2) we can hopefully come up with a decision that
  performs best for the most likely states.
* Basically, of all correct plans, pick the one that maximizes
  expected utility.
* Note that this is interesting philosophically. YOu can have a
  complete model of `if-then`s, but if you don't know the
  probabilities, you won't be able to take reasonable actions. Like,
  if a test result could indicate two things, what do you do? It's
  obvious if you know it's 99:1 for one of the results, but totally
  undecidable without that information. This possibly can be resolved
  with more tests, but what if you can't run those?
* They introduce a bunch of basic probability terminology.
* They mention Kolmogorov's axiomitization. They then ask: why should
  people hold beliefs that correspond to these axioms? Basically, what
  should we mean when we say "probability"? They mention de Finetti,
  who said that if you use the probabilities to bet, views that
  violate Kolmogorov's axioms will lead you to a strategy where you
  lose money all the time.
    * However, it has been pointed out that maybe you prefer to lose a
      little each time, to avoid the possibility of worse and larger
      outcomes. Basically, you are paying insurance against variance.
    * But you could make the amounts arbitrarily small.
* That was about the use of probabilities. They also mention what
  probabilities mean.
    * They mention frequentist, objectivist, and subjectivist views.
    * Objectivists believe that there is an underlying
      propensity. That may be true at the quantum level, but most of
      randomness comes from ignorance of initial parameters, not
      quantum.
    * They mention that frequentists still have to *frame* the
      problem, which is called the *reference class problem*. In this
      way, frequentism still involves some subjective views.
    * They note that you can have any prior beliefs under a Bayesian
      interpretation, so long as you use the right updating rules.
    * Finally, there is the *principle of indifference* (Laplace),
      which says that if outcomes are equivalent in a "syntactic" way
      (like the sides of a die), then accord them equal
      probability. Carnap tries to extend that to be able to speak of
      the probability of any statement. This is interesting because it
      appears to extract probability from *syntax*.
* Terms they discuss:
    * Sample space
    * Unconditional/marginal/prior probabilities. Conditional/posterior
      probabilities.
    * Density functions. Joint distribution.
    * Normalization.
    * Independence/conditional independence.
    * Product rule/Bayes rule.
* They show how identifying conditional independence lets you
  factorize the joint distribution and not have to track of learn the
  full joint distribution.
    * They mention Naive Bayes, and that it can work well even when
      the assumption of conditional independence is not true.

## Ch15: Probabilistic Reasoning Over Time

They introduce *Markov processes*, which of course means that the
state at time `t` is independent of all prior states or observations
given the state at time `t-1`. We assume the state transition
distribution is *stationary*: doesn't change over time. The Markov
*sensor model* assumes that you have have these observations that are
independent of any prior state or observation given the current state.

**Filtering**

If the state space is discrete, this is called a *Hidden Markov
Model*. With an HMM, if you have a belief distribution over the prior
HMM states, and then you see a new observation, it's easy to update
your beliefs about the new states. Basically, you take your prior
states, and applying the Markov transition matrix; this mixes the
states. But then you apply Bayes' rule: each belief probability by the
probability it produces this outcome, and then normalizing.

The task of estimating the current belief state is called *filtering*,
which is a name originating from signal processing
literature. Sometimes this is called *state estimation*.

For an HMM, if you repeat this over `t` time-steps, this process takes
the same amount of time at each time-step. The memory usage is
constant and doesn't grow over time. On the other hand, the matrix
application I suppose takes `O(||S||**2)`.

Now, if the state space is infinite, then I ask: how many parameters
does it take to specify the prior state space distribution? And how
many parameters will it take to specify the *evolved* state
distribution? And how many parameters will it take to specify the
state distribution *conditioned on the evidence*? This is going to
depend on the structure of the initial, transition, and sensor
distributions, and it may not necessarily be tractable.

**Prediction**

Another task is *prediction*, which calculates the distribution after
`t` future timesteps. This basically just does mixing, without any
future evidence observed (because it hasn't happened yet). This is
just a simpler version of what we saw above. It turns out that Markov
processes will converge to a stationary distribution as the period of
ignorance increases. In fact, it's typically pretty hard to predict
very far out.

**Smoothing**

The task of *smoothing* is to look at a past time `t` and find the
probability distribution over states given the prior *and subsequent*
observations. This involves working "forward" as above in the
filtering task, but it also involves working "backward" from the most
recent observation. The backward work involves recursively computing
`Pr(e_{t+1:} | X_t)` which says: "what is the probability of producing
future evidence given various states at time `t`.

Again, this is simple for HMMs; it takes constant time to move
backward each step. In fact, you can smooth the *entire sequence* in
`O(t)` time and `O(t)` space if you go forward recording `Pr(X_t |
prior evidence)` at each timestep and then working backward and
multiplying by `Pr(future evidence | X_t)`. Again, this
*forward-backward algorithm* works for any Markov process, but
specifically we know it is efficient for HMMs.

Another issue is doing smoothing online. In particular, we may want to
keep up-to-date a smoothed estimate of the state `d` time-steps ago;
this is called *fixed lag smoothing*. The idea is perhaps that `d`
steps ago is the last time we'll have reliable estimates (because
subsequent information is very useful in improving accuracy of our
state beliefs). We know this is simple for `d=0` (because that's just
*filtering*), but we would like a solution independent of `d`.

**Most Likely Estimation**

A *most likely explanation* tries to figure out the most likely series
of states that generated the observation stream. Note that you can't
just take the most probable state at each time and call this the most
likely sequence of states. The solution to this problem is solved by
the *Viterbi algorithm*. Basically, at each time step `t`, you track
(1) what's the probability of the most likely sequence of states to
time `t` ending with state `x_t` and (2) what is that sequence?

Note that it is simple to apply Viterbi to an HMM, but I'm not sure
it's easy to keep track of the most probable hidden state sequence if
the distributions are continuous...

**HMMs**

They note a few things. First, they acknowledge that a step of the
forward algoirhtm takes time `O(S**2)` beacuse of the matrix
application.

They note that smoothing *at a single position* can take `O(S)`
space. That makes sense to me: working forward to time `t_0` should
require only an up to date `Pr(X_t | prior evidence)`, and then
working backward `Pr(future evidence | X_t)`, which is `S`
numbers. But they seem to imply that I'm missing something about the
forward-backward algorithm that would make it hard if the transition
matrix wasn't invertible. But I'm not that interested in this.

They also note that fixed-lag smoothing is simple. I believe this
basically involves keeping a circular buffer of the evidence, and
current forward and backward probabilities that you update as you go.

I'm just skimming this because it isn't vital information right now.

**Kalman Filtering**

(I'm up to here)

## Ch16: Making Simple Decisions

* Basic decision theory. Utility function, expected utility, principle
  of maximum expected utility is definition of rationality.
* They define preference relation. Then they begin to consider
  lotteries. Now we have to give a preference relation on the
  lotteries.
    * Axioms are orderability and transitivity. Then continuity (if A,
      B are prefered to C, there is some mix of A and C equivalent to
      B), substitution (if A and B are equivalently preferred, then
      substituting one for the other in a lottery is equivalent),
      monotonicity (if A is preferred to B, prefer a lottery of the
      two with a higher probability of A), decomposition (lottery of A
      and a sublottery B and C can be decomposed into a single lottery
      of A, B, C, with the appropriate probabilities).
* Now, you can prove there exists a utility function expressing
  preferences over results. And you can set the utility of a lottery
  to the expectation of the utility of the result.
    * You can do any affine transformation of the utility function.
    * Sometimes it is called a *value function* since the unit is
      irrelevant.
* They mention the problem of preference elliciation. They mention
  attempts to find units like micromorts or quality-adjusted life
  years.
* They mention that even though there is a monotonic measure for
  money, we can't measure utility in terms of dollars
  necessarily. Because lotteries of money are not worth their expected
  value.
    * In fact, they have found that people tend to value money
      logarithmically.
    * Certainty equivalent and insurance premium.
* They note that people don't act like decision theory says they
  should. They suggest a number of reasons for this. To me, the most
  interesting possibility is the possible use of heuristics.
* They talk about strict dominance of one choice over another. But
  they also talk about *stochastic dominance*. If you look at the
  cumulative distribution in utils for two options, and at the
  `p`-percentile option A is always better than option B, then option
  A stochastically dominates.
* Can we decompose a multi-variate utility into several parts?
    * Hopefully! One form of structure is *mutual preferential
      independence*. If we prefer `a, b` to `a', b'` in the case of
      `c`, then hopefully we prefer `a, b` to `a', b'` for any other
      `c'`.
    * In the case of MPI, we can decompose the value function into an
      additive sum of independent value functions.
* MUI is mutually utility independent. It extends MPI to lotteries:
  that a preference for a lottery #1 of `a` and `b` to lottery #2
  doesnt depend on a value of `c`.
    * In that case, apparently you can decompose the utility into a
      multiplicative function.
    * With some additional assumptions, this can even be decomposed
      linearly.
    * Note the use of the word *utility* when lotteries are involved,
      vs *value* for when only preference over atomic outcomes.
* Influence diagrams (which they call decision networks) are like
  bayesian networks except there are choice nodes too. And there are
  utility nodes (sometimes called value nodes).
* They talk about the *value of information*.
    * One of the points of these networks is when you don't know all
      the random variables.
    * How much is it worth to you to conduct a test to know a
      variable?
    * They give an example: you have `n` oil-drilling sites; one has
      oil, the others don't. How much do you pay to test one of them?
    * In general, you calculate (1) the expected utility under your
      best decision making, (2) you calculate the expected utility
      when you know the value of `X=x_i` (for each `i`), note that
      your decision making may change, (3) you calculate the
      differences of (2) from (1), and (4) you weight by the
      probability fo the `x_i`.
    * Value of information can be low if it won't really effect the
      best play, or (2) it might effect the best play, but the change
      in expected value is low.
* A *myopic* information gathering agent will ask questions/perform
  tests where the expected value of the information exceeds the cost.
    * This is a greedy approach.
    * Maybe a pair of questions has a lot of value, but not
      individually?
    * Still, greedy systems tend to better than real doctors.
* They describe how to put together an expert system.
    * Ask a domain expert about causes. Determine what actions can be
      taken. Assign probabilities of outcomes to causes.
    * Actually, if you have symptoms and need to reason backward, you
      want probabilities of causes from outcomes. But it's hard to ask
      people for that information, so it's the system's job to invert
      these probabilities.
    * We need to assign utilities to the outcomes.
    * Last, we can perform *sensitivity analysis*. Here we tweak the
      probabilities slightly to see if our decision making is
      sensitive to these changes. If so, then we want to collect more
      dta about those variables.

## Ch17: Making Complex Decisions

* They're going to consider *sequential decision processes*. That
  includes games.
* You take an action, but outcome is probabilistic. Thus there is a
  distribution `P(s'|s, a)`. In the case where the state space is
  discrete you can represent this as a rank-3 transition tensor.
* For now, we will assume the agent receives a reward `R(s)`. The sum
  of the collected rewards is what the agent wants to
  maximize. Specifically, the expected sum.
* This is called a *Markov Decision Process* or MDP. That's because of
  the Markovian transition matrix.
* If there were no stochastic transitions, a solution would be an
  action sequence. Because the transition is stochastic, the solution
  is a *policy* of what to do in each state. Optimal policy maximizes
  the expected total reward.
* If the game has a fixed time horizon, note that the optimal move in
  a state might depend on how much time is left. If the horizon is
  infinite, then it won't matter.
    * Infinite time horizons can lead to policies that never end, or
      they may contain a terminal state, and the policy might say go
      there.
* Next, how do we combine the rewards from the states? One way is to
  add them. Could we do another?
    * If we assume that if `R(s_0, ...) > R(s_0', ...)`, and
      `s_0=s_0'`, then we ought to have `R(s_1, ...) > R(s_1',
      ...)`. We say the preferences over state sequences are
      *stationary*.
    * This seems like a simple assumption. Note however that it is not
      true if there is a finite time horizon.
* It turns out that stationary preferences over state sequences
  implies the form `\Sum \gamma**i R(s_i)`. Note if `\gamma=1.0` this
  is just additive reward.
* If no discounting is performed, and if positive rewards can be
  picked up over all time, and there are cycles, the agent may just go
  in circles to get total reward infinity.
    * So discounting gives a bound.
    * On the other hand, if there are terminal states and the agent
      will enter one eventually, we're okay. That can happen because
      (1) the agent decides to or (2) the agent can't avoid it because
      of stochastic transitions.
* For each starting state there is an optimal policy which determines
  the next action from the current state. The optimum maximizes the
  expected total discounted reward. It turns out that if the horizon
  is infinite, then the optimum policy is independent of starting
  state! That makes sense, actually; I don't find that surprising.
* Given the utility function, we can choose the best action `a` at `s`
  to maximize: `\Sum P(s'|s, a)U(s')`. Note however that this
  definition is somewhat circular, as `U(s')` depends on the best
  policy.
* As mentioned, we can flip this around to the Bellman Equation. This
  says that `U(s) = R(s) + max_a \Sum_{s'} P(s'|s, a) U(s')`.
* We would like to solve this. If this were `n` linear equations with
  `n` unknowns, life would be easy, and we could just use linear
  algebra. But `max` is not linear, so we must do something else. One
  way is *value iteration*, which involves random initialization and
  then doing the recursive update until stabilization.
    * Apparently this is guaranteed to converge to an equilibrium, and
      moreover there is a single equilibrium.
* They prove this.
    * A contraction function has `|f(x) - f(y)| < \gamma|x -
      y|`. Notice the `\gamma`; we need to contract by at least a
      fixed amount.
    * A contraction can only fix at most one point. Else `f(x)` and
      `f(y)` wouldn't get closer.
    * Any other point must move closer to the fixed point each time.
    * Actually, they didn't prove there is *one* fixed point!! This is
      the Banach Fixed Point Theorem which holds for any complete
      metric space. But I'll take their word for it...
* You next need to show that `|(max f(a)) - (max g(a))| <= max |f(a) -
  g(a)|`. That is actually quite simple.
* Then show that `||B(U) - B(U')|| <= \gamma ||U - U'||` in the
  infinity norm (`B(U)` means the Bellman update to `U`). That is
  pretty straightforward. This establishes that the Bellman update is
  a contraction mapping by a factor of `\gamma`. Per Banach this
  converges.
* Note that the contraction by a factor of `\gamma` means we reduce
  the *error* to the fixed point by a factor of `\gamma` each
  time. This is very fast convergence. This is called *exponential
  convergence*.
* What we see is that the convergence gets much slower as we increase
  `\gamma` toward one. That's unfortunate because it effectively makes
  our policy more myopic.
* Note something nice. If the update makes a small difference, we can
  actually say that we must be close to the optimum. That's because
  the update is supposed to contract the error us by a factor of
  `\gamma`. So if the update change is at most `\eps\gamma`, then the
  error to the equilibrium was at most `\eps`.
* Note also that the best policy `pi` may become optimum long before
  we actually converge on the correct utilities! The utility estimates
  may be inaccurate, but not in a way that affects the best policy.
* This insight leads to *policy iteration*. The idea is this: if `a`
  is clearly the best action at state `s`, we don't need to get better
  and better estimates on the utilities of successor states.
    * So pick a policy. Calculate the utilities given this policy.
    * Next, pick the best policy given these utilities.
    * Stop when the policy doesn't change.
* When the policy doesn't change, the associated utilities are a fixed
  point for the Bellman updates. So this is an optimum.
    * Note, policy space is finite, and we keep getting better
      policies each time, so algorithm must terminate.
    * Actually, we haven't shown that each policy is better than the
      last...
* One thought: how do we calculate the utilities under a given policy?
    * Well, this is just `n` equations and `n` unknowns. Thus it can
      be solved with linear algebra in time `O(n**3)`.
    * Note that things are even simpler if there are no cycles.
    * Note that we couldn't do this with the value iteration approach
      because we needed to consider the best action at each
      value. Here, we just use the action specified under the current
      policy.
* I'm not sure it's clear to me whether to prefer policy or value
  iteration.
    * With value iteration, you do a Bellman update repeatedly. Each
      iteration needs to update utilities for each of `||S||` states,
      consider `||A||` actions, and summing out a distribution of
      utilties of each of `||S||` successor states. That's
      `||S||**2||A||`.
    * With policy iteration, you don't consider all possible actions
      at every state. Instead, you have `||S||` equations of utilities
      to calculate, each based on `||S||` other utilities. If there
      were no cycles this could be solved in `||S||**2` I think, but
      with cycles you need to do linear algebra which is `||S||**3`.
    * Thus, I believe policy iteration is preferable when the number
      of successor states is smaller than the number of actions?
    * However, the next question I'd have is what about the rates of
      convergence?
* A hybrid approach seems to be empirically the best. You do policy
  iteration, but instead of solving the linear system (which is
  `O(||S||**3)`, you just do `k` iterations of (simplified) value
  iteration, which should get you fairly close to the actual
  optimum. Hopefully close enough to then update the policy correctly.
* They mention but don't discuss strategies where you don't update all
  the values for all the states, but just focus on those states which
  seem to be important. This is called *asynchronous policy
  iteration*. The intuitive idea is: if you can see that jumping off a
  cliff is stupid, it doesn't really matter what the successor states'
  values actually truly are.

**17.4 Partially Observable MDPs**

Here, you don't know exactly what state you're in. That sucks, because
it makes everything harder. But unfortunately this is a more realistic
model.

The major change in POMDPs ("pom-dee-pees") is a *sensor model*. You
still have actions, you still have states, you still have
transitions. We assume you know the actions, you know the transitions
probabilities. The only thing you don't know is your current
state. But you do have a sensor which gives you *evidence*. You know
`P(e|s)`.

The agent keeps track of a belief state which is a probability that it
is in each of the states.

**I paused here to review Ch15**

## Ch18: Learning From Examples

* 3 reasons for learning. Can't anticipate all future scenarios;
  future distributions may shift away from current distributions;
  sometimes fundamentally don't know how to program a solution (like
  how do we recognize faces?)
* The mention a number of ways to train:
    1. Tell the agent exactly what to do in circumstances. Specify.
    2. Give examples of what should be done in many training examples,
       and see if the agent can infer.
    2. Experiment: what happens when I try X?
    3. Rewards: Do the stuff that gives you a reward.
* Unsupervised, Supervised, Reinforcement. Semi-supervised is when you
  have a bunch of unlabeled data and some labeled data.
* In supervised learning, we get examples in a training set, we try to
  learn the function `f` that relates the `y` with the `x`; what we
  infer is the *hypothesis*. We test the accuracy of our hypothesis on
  the *test set*. This is a test of the *generalization* of our
  hypothesis. The hypothesis may be an estimation of a probability
  distribution `Pr(Y|x)`.
* Classification vs regression.
* *Hypothesis space* may contain many hypothesis *consistent* with the
  data. How to choose? They suggest *Ockham's Razor*, but note that
  defining simplicity is not easy. They note that it is in general
  impossible to know whether the hypothesis space contains the correct
  model.
    * They mention a way to do this is Bayesian, by giving model's
      prior probabilities.
    * They do mention that the *expressiveness of the language* can
      make it easier or harder to express a model. They specifically
      give chess as an example: definable in a few pages with first
      order logic, but needs hundreds of pages for propositional
      logic.
* They mention that we can't use all TMs as our hypothesis space
  because there is a trade-off between learning complexity and the
  expressiveness of the hypothesis space. Fitting arbitrary TMs is
  undecidable.
    * They give a second reason: they want a model fast to evaluate in
      the future, and a TM may be too slow.

**Only read to decision trees; this actually looks useful!**

## Ch21: Reinforcement Learning

* For a Markov Decision Process, the best action is the one that
  maximizes the expected future reward. If the actions and the rewards
  and the transition probabilities are known, then you can calculate
  the best *policy*. But if you don't know everything, you must learn
  the policy. That is the task of reinforcement learning.
* Reinforcement learning may be the only feasible way to learn when
  humans cannot give good evaluations of states.
* They first talk about *passive learning*. Here, the policy is
  fixed. The agent doesn't know the transition model though: `P(s'|s,
  a)`, but they have fixed their chosen action `a` for any state `s`.
    * This is basically *policy evaluation*.
* For data, the agent can execute a series of *trials* of the
  policy. Because of randomness in the transition, each trial can be
  different.

**Passive Reinforcement Learing**

* The simplest approach is *direct utility estimation*. This considers
  each position, and calculates the average future reward from this
  position whenever it appeared in the various examples.
    * You can actually use whatever modelling technique you want.
    * In the limit the empirical mean of future rewards for a state
      `s` will approach the true expected utility of this state under
      the policy.
* However, note the Bellman equation:

    U_\pi(s) = R(s) + \gamma \Sum P(s' | s, \pi(s)) U_\pi(s')

This equation gives the utility of the state `s` under policy `\pi`,
which is the reward *at* `s` is `R(s)`, plus the discounted (`\gamma`)
reward of the subsequent utility of state `s'`, which we transition to
with probability `P(s' | s, \pi(s))`, where `\pi(s)` is the action we
take.

Ideally, our hypothesis space will be those functions that obey the
Bellman equation. The direct utility estimation method, especially if
we use just the empirical mean, can lead to results that are
inconsistent with the Bellman equation. While the direct utility
estimation method will converge to the correct answer, it may do so
slowly because it needs to consider/reject theories which shouldn't
even have been allowable in the first place.

An *adaptive dynamic programming* approach just tries to estimate the
transition probabilities. Then it can solve using regular dynamic
programming for the Bellman equation. They talk about how you can do a
MLE by computing `#(moved to state s' from s via a) / #(took action a
at state s)`.

I misstated passive learning. You aren't locked into a single
policy. It's the data that's fixed (you can't play the game while
learning). But once you estimate the transition probabilities, you can
try to pick the best policy.

They suggest another idea as opposed to picking the best policy in
light of the MLE transition probabilities. This is *Bayesian
reinforcement learning*. You view some evidence, and you have
probabilities for various hypotheses about the transition probability
matrix. Now you pick the best policy which maximizes the expected
utility under this collection of theories.

Another approach is *robust control theory*. Here you keep a subset of
the hypothesis space of theories with a baseline likelihood (you throw
away the rest). Then you choose the policy that maximizes the
performance under the most adversarial hypothesis.

A crude version of ADP is iterate over the examples. At each state
`s`, consider a successor state `s'`. Consider the difference between
`U(s)` and `U(s')` (the `U` are your current estimates of state
utility; they can be initialized randomly). Add `\gamma
difference_in_U` to `U(s)` to update this; the `\gamma` is a learning
rate. To get this to converge, you need take the average of `U(s)`
which involves keeping counts of how many times you've been at `s`. Or
you can simply decrease `\gamma` over time.

This is called *temporal-difference learning* (stupid name). The
"temporal" is that you are comparing the difference in utilities of
two states at different times. This approach doesn't try to learn the
transition probabilities. Basically, it does ADP, but one example at a
time (rather than considering all possible actions).

On the other hand, you can make the refinement finer by avearging over
several examples. They also note that learning the entire transition
matrix can be unfeasible. So they recommend picking just a few of the
most relevant examples. This is called *prioritized sweeping*.

They talk about "adjustments" made by ADP. I assume they are talking
about the trouble with cycles, since otherwise the dynamic programming
solution is exact and needs to evaluate each vertex once?

**Only read to Active Reinforcement Learning**
