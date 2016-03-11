* Average-case time complexity assumes you have a probability
  distribution over inputs. Expected time complexity says that you
  have a *randomized* algorithm that uses random bits. E.g., quicksort
  without random pivotes has average-case linearithmic time
  complexity, but *with* randomized pivots has expected linearithmic
  time complexity.
    * It's an almost pedantic point.
* Often we can convert an average-case algorithm to an expected time
  algorithm by randomizing the input so that it matches the presumed
  input distribution.

Randomized algorithms are algorithms that use randomness: they come in
two major types. There are algorithms which use randomness to acheive
a better expected runtime than deterministic algorithms, but always
get the right answer (e.g., quicksort). These are sometimes called
*Las Vegas* algorithms. ZPP are those Las Vegas algorithms with
expected polynomial runtime. You can see more discussion of ZPP in the
Aaronson notes.

Another group of algorithms are those which use randomness to produce
an answer that is approximately correct, up to some error bound. These
are called Monte Carlo algorithms. These are sometimes called
*probabilstic algorithms* or *approximation algorithms*.

There is actually a third use of randomization, which is to defeat
adversaries that might try to exploit determinism in your
algorithm. This is the case, for instance, in game theory strategies.

Any Las Vegas algorithm can be turned into a Monte Carlo algorithm by
setting a time bound and outputing a random number if the answer is
not computed quickly enough. Likewise, a Monte Carlo algorithm can, if
an efficient verification procedure exists, run repeatedly, each time
checking its answer.

Note that `BPP=P` is a conjecture, which would indicate that
randomization does not give us any additional ability to efficiently
compute. Well, that may not be totally true; it is relative to a TM
model. `BPP=P` is conjectured *for a TM*. For a random-access machine,
maybe `BPP!=P`. I'm a little confused on this point... But it does
seem like even though a RAM is TM-equivalent, that doesn't mean the
same set of problems are efficiently solvable...

NB: This is probably wrong. Sources indicate that any problem solvable
in `O(n**k)` time on a RAM is solvable in `O(n**ck)` for a TM.

It doesn't look like randomized algorithms are crazy
interesting. Approximation algorithms, OTOH, let you solve problems
you otherwise might not have any way to efficiently solve.

## Approximation Algorithms and Complexity

The *approximation ratio* is `C/C*`, which is the cost of the solution
found by the algorithm versus the optimal solution. We can do an
asymptotic analysis of this wrt problem size. Sometimes there are
polytime solutions with constant approximation ratios. Sometimes the
best known algorithm has an approximation ratio that grows with the
problem size. And some problems allow you to trade off the
approximation ratio with time. We typically call this an approximation
scheme; we specify the approximation ratio by `1+\eta`. A polytime
approximation scheme has runs in polytime for any eta. But the runtime
can grow very fast in eta.

A *fully polytime approximation scheme* is polytime in both `n` *and*
`1/e`. This means you can trade off runtime for accuracy in a
polynomial way.

From a complexity point of view, some `NP`-hard problems have fully
polytime approximations, whereas others are provably impossible to
approximate to within even a factor polynomial in the input size
unless `P=NP`. APX refers to those polytime problem with an
approximation ratio bounded by a constant. APX is a strict superset of
PTAS if `P!=NP`; there are (expected to be) problems that cannot be
polytime approximated within any constant factor!

Note that a PTAS/FPTAS typically refers to *deterministic*
algorithms. You can also consider randomized algorithms schemes. These
are sometimes called PRAS/FPRAS.

You might consider FPRAS a more natural conception of what you can
compute, versus ZPP.

## Relevant Problems

* Metric K-Center (how to place k shipping centers closest to a bunch
  of cities).
* TSP
* Max 3SAT (try to find an assignment satisfying as many statements as
  possible).
* Min k-Cut (find min-weight edge set to cut graph into k components).
* Nearest Neighbors

## Vertex Cover

Take a graph, you want to find a minimum set of vertices such that
every vertex is either in the set, or adjacent to a vertex in the
set. The decision version of the problem is NP complete.

Here's an approximation algorithm: take an edge randomly, add both
vertices to the cover, and remove all edges incident on either
side. Repeat until empty. By the time the algorithm completes, we have
a vertex cover for sure. It is clearly polytime.

We argue this is a 2-approximation algorithm. Consider the set of
edges selected through the algorithm and any vertex cover. Every
vertex selected must be an endpoint of exactly one edge in the set. If
it is not incident to any edge, then we didn't produce a vertex
cover. And it can't be incident to more than one edge since we removed
all edges incident to each side.

Reflecting on this proof, we see that we were able to prove an
approximation ratio without knowing the best answer. We did this by
use of a *lower bound* on the ideal answer. We knew that it couldn't
be better than our solution by half. In fact, our solution might be
better!

## TSP

Many versions: metric, Euclidean are both NP-complete still, but allow
for different approximation algorithms. The Christofides algorithm is
the best approximation algorithm on metric spaces (`\eta=1/2`). It is
proven that there exists no PTAS for metric TSP if `P!=NP`. Also,
there is no constant approximation algorithm for general TSP if
`P!=NP`. Sanjeev Arora recently found a PTAS for Euclidean TSP:
`O(n(log(n))**(O(c)))` for a `1/c`-approximation. So this is not that
great for better and better refinements.

A simple approximation for metric TSP (with full connectivity) is to
generate the minimum spanning tree. This is polytime. Then consider a
pre-order walk. This is not a tour; it goes up and down, revisiting
vertices and re-traversing edges. So form a tour that visits each
vertex in the pre-order, but skip repeated locations.

I argue this is a 2-approximation. Take an optimal tour; delete one
edge. This is a spanning tree, not necessarily the minimum. So we know
the MST is less than the cost of the best tour.

Consider the tour generated by the "full walk" of the MST. This is a
tour, and since it visits every edge of the MST twice, it has double
its length. Therefore, it is no more than twice the length of the best
tour.

Except we're not allowed to revisit nodes in a tour. Good. Then do the
modification as described above, going direct to new nodes, rather
than passing through old ones. This *reduces the cost*, so we're *even
better* than revisiting nodes.

So the tour is no worse than twice the cost of the best tour.

**No Polytime Constant-Approximation Algo for General TSP**

First, note that the existence of a Hamiltonian circuit is NP
complete. Let us assume we can solve TSP within `\eta` accuracy in
polytime.

So take a graph. We want to know whether it has a Hamiltonian
circuit. We extend the graph by making it complete, each new edge has
cost `\eta|V| + 1`, while each edge of the old graph has cost
`1`. Solve for TSP. If a Hamiltonian circuit exists, then your
solution can't use any of the additional edges, since every each new
edge is so costly it costs more than `eta` times the cost of the
Hamiltonian path.

Therefore, we've used an approximation algorithm for general TSP to
solve Hamiltonian path, which we can't do unless `P=NP`.

**What About Randomized Algorithms?**

Using randomness could help in two ways. First, it might mean that
degenerate cases which take a long time would be less likely to take
so long, letting us use more powerful techniques. Also, we wouldn't
have to consider the very worst outcome possible from the
approximation, we could consider the expected approximation factor.

Norvig (http://norvig.com/ipython/TSP.ipynb) talks about using nearest
neighbors to approximate TSP, with randomized starting points. He also
improves this by iterating through the your and "reversing a segment",
altering and helping his tour. He also considers a "greedy" strategy;
divide the graph into paths (starting of length 0). Pick the shortest
edge that would connect two paths end-to-end. This is more "global" in
some sense.

He compares these approaches to the MST solution. His greedy,
heuristic solutions are faster and produce better results. Of course
they have no guarantee, but it's impossible to analyze an expected
case (since what's the distribution of points?).

## Knapsack Problem

There exists a FPTAS for knapsack: find the load that has greatest
weight without exceeding a limit.

Obviously one way to solve the knapsack problem is to generate all the
subsets. This is exponential time. One way to generate all the subsets
is to generate all the subsets of `{x_1,...,x_i}`, then tack on
`x_{i+1}` to each and union. Note that you can drop subsets that are
too weight along the way. This seems like an okay way to get the right
solution.

So here's the idea. Let's "trim" the set along the way. Choose a
parameter `\delta` such that we drop every item `y` where there
remains an item `z` such that `y/(1+\delta)<=z<=y`. Basically, we're
saying that `z` approximates `y` well.

I claim we want to set `\delta=\eps/2n`, where `\eps` is the desired
approximation ratio. We know that the approximation ratio is
`(1+\eps/2n)**n`, since the error compounds for each element. It turns
out that this number is less than `1+\eps` (you can prove it with
math).

To show this is polytime, note that the number of elements in the list
is logarithmic in the size of the knapsack capacity (since each
element is `<(1+\delta)` times bigger than the last). So each step
looks at a logarithmic number of items.

Note also that, as we change `\eps`, we change the base of the
logarithm. But by virtue of the equation for change of base of
logarithms, we can show that this grows polynomial in `1/\eps`. I
don't do so here.

## General Strategies

* Repetition: Keep trying with random parameters.
* Alteration: Take some solution, and keep making local improvements.
* Greedy: Keep doing the "locally best" thing.
