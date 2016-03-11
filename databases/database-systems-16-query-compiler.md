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
    * I guess we'll talk about join execution later.
* Given that a number of plans are possible, they talk about how you
  might search.
    * Heuristic guided.
    * Branch-and-bound: start with a decent plan, to cutoff and stop
      considering plans that exceed that cost.
    * Can try hill climbing.
    * Can also do a dynamic programming style where we find the best
      plan for a smaller part.
    * "Selinger"-style optimization: dynamic programming, but also
      keeps some other, higher-cost possibilities sorted such that it
      might help further up the tree.
* They actually have pretty poor discussion of how to select amongst
  the options. This chapter hasn't been too useful...
