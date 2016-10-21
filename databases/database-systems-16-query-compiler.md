* First talks about how to parse the query for a little bit.
* Talks about how to transform the parsed query into a relational
  algebra expression.
* Talks about a bunch of laws that apply to transform queries.
    * For instance, how to push down where's deeper.
    * Or combining commutative operations.
    * These allow us to improve the expression.
    * They mention that we have to pull out subqueries from
      conditions, transforming this to joins. I always suspected that.
    * However, we haven't yet discussed *join ordering*.
    * The result so far is the *logical plan*.
    * When we figure out how to order the joins (and how to execute
      them) we'll have a *physical plan*.
* It then talks about how you might estimate costs. Mostly it focuses
  on the number of records produced by joins.
    * In particular, they say that since the worst case is
      `Card(R)*Card(S)`, we need to scale this by the join
      selectivity.
    * Consider the number of values of join key A in R and S, denoted
      `V(R, A)` and `V(S, A)`.
    * They'll assume that all the values of one side are contained in
      the other. That makes sense if one side is foreign keys and the
      other primary keys.
        * They argue that this is usually approximately true.
    * In that case, let `R` be the primary key side. Then the
      probability it has someone to join with is `V(S,A)/V(R,A)`.
    * Let's also say that `S` values are uniformly distributed. So the
      number of records a row will join to, if any, are
      `Card(S)/V(S,A)`.
    * Therefore, the # of records a row of R will join with are
      `Card(S)/V(R,A)`.
    * But we multiply this by # of rows in R to get
      `Card(R)*Card(S)/V(R,A)`.
* Given that a number of plans are possible, they talk about how you
  might search for an best join ordering.
    * Heuristic guided.
    * Branch-and-bound: start with a decent plan (from heuristics), to
      cutoff and stop considering plans that exceed that cost.
    * Can try hill climbing. Start with decent plan and try to slowly
      improve.
    * Can also do a dynamic programming style where we find the best
      plan for a smaller part and combine these up.
    * "Selinger"-style optimization: dynamic programming, but also
      keeps some other, higher-cost plans that result in interesting
      sort orders.
* One-Pass Join Types:
    * Hash join. We'll assume the smaller relation (which we build the
      hash table out of) is on the left.
    * Index Join: assume the right relation has the index. We go
      through the left, probing the right relation.
    * Nested-loop join: again, assume we are iterating through the
      left, and the inner loop is probing for that value in the right.
* Join Trees:
    * Left-deep (only left children), right-deep (only right
      children), and bushy (intermediate nodes results of joins).
    * It is typicaly just to consider the left-deep trees.
    * First, this limits the search space and makes the problem more
      tractable.
    * Also, they claim that there is usually a pretty good plan
      amongst the left trees; you won't always find the best plan, but
      you can normally come close.
* Why does left-deep tend to be efficient?
    * For one-pass joins (Hash, Index, Nested-loop), the amount of
      memory used tends to be less.
        * Imagine joining four relations. All else equal, if you join
          one at a time, you only keep one join result in memory at a
          time.
        * If you join pairs, and then join the pair results, then you
          need to simultaneously store both pairs.
        * Basically, the bushy tree is *wide*, so it means you need
          more memory to hold intermediate results. Or flush to disk,
          but that's bad, of course.
        * If we assume that left operand in a one-pass algorithm is
          kept in memory, then right deep is the worst. To join A,B,C,
          we need to first bring A into memory to join with `Join(B,
          C)`, but then to produce `Join(B, C)` we need to bring B
          into memory. So we have to bring n-1 relations into memory.
        * Of course, there's no benefit to this, right deep trees are
          dumb and are dominated by their mirror image.
    * With nested-loop joins, right deep or bushy will involve
      producing intermediate relations repeatedly.
        * Assume right deep.
        * If joining A,B,C, then as we iterate through A, will have to
          scan `Join(B, C)` repeatedly.
        * But of course if we have to keep reconstructing this, that
          sucks.
        * We could write the intermediate result to disk, but that
          will involve more IO of course.
* Dynamic Programming
    * Calculate best way to join any 2 relations.
    * Next, find the best way to join every 3 relations by taking the
      three best ways to join two of them and try attaching the third
      as the last join. Pick the best.
    * For four, consider the best way to join three of the four (there
      are four of these), and then join the fourth relation. Pick the
      best.
        * This is for left-deep trees.
        * Otherwise, you also need to consider 2+2 and 1+3.
        * Obviously will be much faster for just left-deep trees.
    * Continue!
    * Can add Selinger style optimization.
    * Join selectivity is important in deciding what ordering will be
      best. That's how we estimate the size of intermediate relations
      produced.
* Greedy algorithm
    * Join in order of selectivity.
    * INGRES does this.
* Brewer says that System R technique is pretty common.
    * That's the dynamic programming one.
    * Says mostly we want to just avoid horrible plans.
    * Says INGRES uses nested loop joins, but orders with smallest
      relation first.
    * Says System R technique only scales to 10-15
      joins. Garcia-Molina says 5 or 6.
    * At that point, it becomes more sensible to use a greedy
      approach, as INGRES does.
* They then talk about how to do a physical plan.
* For a select with a condition you can:
    1. Read every tuple and decide whether to filter. This involves
       reading every block of the relation.
    2. If it's an equality constraint and you have an index, then
       there are `1/V(R,a)` tuples on average that will meet the
       criteria. If the index is clustering, you read only this
       proportion of the blocks. But if the index is *not* clustering
       (typically not!), then you read `T(R)/V(R,a)`, where `T(R)` is
       the number of tuples. So this can be worse!
    3. Same idea (but different selectivity factor) if using an
       inquality constraint.
    4. When there are multiple column criteria, they only talk about
       how to use one index (and ignore the others), but you could get
       fancy...
* Choosing a join method:
    * Index join is ideal if index already exists on one relation.
    * Sort join is good especially if the sort order can be used
      again. Or if the operands are already sorted.
    * Else, hash join is probably best. Don't know why you would ever
      choose a nested-loop join...
* Pipelining vs Materialization
    * Naive way is to execute each step of plan entirely.
    * Alternative is to *pipeline*. Especially makes sense for
      projection followed by a join. The projection can happen a
      block-at-a-time and be provided to the join algorithm.
    * Pipelining can increase memory pressure though, by trying to do
      several operations at once.
    * So it's a balance, it can save IOs by not needlessly flushing to
      disk, but at the cost of using more memory. If you run out, then
      you're fucked...
    * Pipelining is typically implemented as a network of iterators.

Sources:

https://people.eecs.berkeley.edu/~brewer/cs262/queryopt.html
