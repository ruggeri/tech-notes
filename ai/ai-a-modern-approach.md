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

## Constraint Satisfaction Problems

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
