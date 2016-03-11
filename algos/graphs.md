# Graph Algorithms

## Topological Sort

Boring. Count the number of in edges and store in a hash. Each round,
pick a vertex with no in edges. That's the next vertex. Decrement the
vertices which have an in edge here. `O(|V| + |E|)`.

## DFS, BFS, IDDFS

DFS can be implemented with a stack, BFS with a queue.

BFS is optimal, and will eventually find a solution if it exists
(*complete*). DFS is not necessarily optimal, and in an infinite graph
can go down a wrong path forever; it also has problems with loops.

DFS is much better at memory usage: it runs proportional to depth of
the tree, while BFS uses `O(b^d')` memory, where `d'` is depth of the
best solution.

You can fix the BFS space problem by running iterative-deepening DFS,
which just performs a depth-limited DFS search for increasing
depths. This is a complete and optimal solution. Also, since most
vertices are at the last level, this still runs in `O(b^d')` time!

**Backtracking**

* You can give up on a branch if you realize it can't possibly lead to
  success.

## Uninformed Shortest Path: Dijkstra and Floyd-Warshall

Dijkstra's algorithm just grows paths by adding path extensions one at
a time. There are three approaches:

* Keep track of best paths to all `V` vertices: `O(E + V**2)`.
* Use a binary heap: `O((E + V) * log(V))`.
    * Worse for dense graphs.
* Use a Fibonacci heap: `O(E + V * log(V))`.
    * Updates are cheap!
    * Best of both worlds, except probably slower than either because
      Fib heaps are slow and complicated.

If you want to find all-pairs shortest paths, you can run Dijkstra's
`V` times. However, if you have a dense graph, you might also use the
Floyd-Warshal algorithm:

* Dynamic programming algorithm that loosens a constraint of only
  traveling through a subset of vertices.
* `V` runs: each time, you select a vertex, updating `V**2` paths.

In a dense graph, Floyd-Warshall has better practical performance than
Dijkstra. But for a sparse graph the repeated Fibonacci Dijkstra is
better. *Except that Floyd-Warshall can deal with negative edges*.

Since I don't give a shit, I don't talk about *Bellman-Ford* or
*Johnson's Algorithm*, which can handle **negative cycles**. Fuck
that.

In the case of trees with a branching factor of `b`, then the longest
path you must consider is of length `C/eps`, where `eps` is a lower
bound on the cost of an edge, while `C` is the cost of the best
path. Therefore, we can take `V=C/eps`.

## Informed Search: A*

**A\*** is a souped-up Dijkstra's algorithm. It requires an
*admissable heuristic*: one which is optimistic about the distance
from a vertex to the target. In the priority queue, we take into
acount not only the path's cost, but the expected further distance to
travel to a goal node.

Generally speaking, this is the same as Dijkstra, which just has a
heuristic of `0`. The algorithm is complete and optimal. It will give
up on bad paths eventually as it slowly realizes the heuristic was
optimistic.

**Correction:** You need a *consistent* (AKA monotone) heuristic. This
one has the property that:

    h(n) <= c(n, n') + h(n')

for all successors `n'` of `n`. The reason this is important is that
when you expand a node, you need to know that you were considering not
just what you think is the probable best path to the target, but in
fact *the best path to that interior node*.

**Result**: When the search is in a tree AND the error of the
heuristic function is logarithmic, then the time complexity is
polynomial! **TODO2: Prove me!**

Note that A* is optimal for any heuristic searching out from the root,
because it needs to consider exactly those paths it considers, lest it
miss a shortcut to the target. Of course, there are other possible
algorithms that don't just start from the root...

NB: A heuristic that dominates another (`h_2(n) > h_1(n)` for all `n`)
will yield better performance, assuming of course it is still
acceptable.

## IDA*

Both Dijkstra and A\* both suffer from very large memory requirements.
Another approach is to modify IDDFS to use a heuristic. In this case,
you set a `bound` for total path cost; you then search in a DFS
manner, abandoning a path when the heuristic total cost `g(a) + h(a)`
greater than the bound.

Like IDDFS, this runs in multiple rounds. Start the bound at zero.
Abandon paths as above; but also keep track of the cost of the minimum
abandoned path. In the next round, use this cost as the new bound to
ensure progress.

```ruby
class IDAStarSolver
  def run(source)
    bound = h(root)
    loop do
      result = search(root, 0, bound)
      return result[:found] if result.has_key?(:found)
      bound = result [:abandonment_cost]
    end
  end

  def search(node, cost, bound)
    if cost + h(node) > bound
      # Abandon this route!
      return { abandonment_cost: cost + h(node) }
    end

    return { found: node } if is_goal?(node)

    abandonment_cost = nil
    node.out_edges.each do |edge|
      new_cost = cost + edge.cost
      result = search(edge.to_vertex, new_cost, bound)

      # May have found it!
      return result if result.has_key?(:found)
      # May have given up!
      if abandonment_cost.nil? || result[:abandonment_cost] < abandonment_cost
        abandonment_cost = result[:abandonment_cost]
      end
    end

    return abandonment_cost
  end
end
```

This returns an optimal path, with much reduced memory usage
(proportionate) to length path. However, it may have higher
computational overhead because of the necessity of revisiting
nodes. It also in some sense doesn't use enough memory; it just
remembers a ceiling heuristic value.

An alternative is **SMA\***, which expands the graph at the leaves in
an `A*` manner. When it runs out of space, it drops the low cost leaf,
writing into its parent that we should return when we've explored all
nodes of lower cost. The parent can be added to the "leaf" set. In
this way, we don't need to keep revisiting early parts of the tree as
we do in `IDA*`.

## Bidirectional Search

Start from both ends and work toward the others. When you link up, you
have a path. You can use BFS or IDDFS in which case the result will be
optimal. You need to keep one of the fringes in memory, so space usage
is `O(b^d)` if we're talking a tree.

On the other hand, you can use A* targeting the other source. It seems
that the result may not be optimal. There is also a concept of
"front-to-front" search where a heuristic exists to move toward the
*front*, and not the *back*. This may be optimal. However, this is
pretty expensive.

## Minimax, Alpha-Beta Pruning

Minimax strategy is to make the best move, assuming your opponent
makes the worst move for you and you respond with the best. Basically,
you try to maximize, then they try to minimize...

You can of course do this to completion with a `+1` for winning and a
`-1` for losing (and maybe a `0` for drawing). This of course doesn't
work for deep game trees, so we use heuristics to guide the
search. For instance, look 12 moves ahead and then apply the
heuristic.

Imagine that the minimizing player sees they can force a loss by
taking the first branch of the move tree from a given position. Then
there is no need to consider the other branches. Alpha-Beta pruning
extends this logic. If the maximizing player sees they can force a
score of `x` on the first branch, then they need to check the other
branches. However, as soon as the minimizing player realizes they can
force a score less than `x`, they can abandon this path, because
things can only get worse for the maximizer, and thus they will not
take this path.

Let alpha be the maximum score that the maximizer knows they can
force, where beta is the minimum score that the minimizer knows they
can force. As the search plays out, alpha and beta will approach each
other until they become equal. To start, alpha may be `-1` and `+1`,
since we know that the maximizer can't do worse that lose, and
vice-versa.

```ruby
def search(node, player, depth, alpha, beta)
  return { path: [node], heuristic: h(node) } if depth == 0

  if player == true
    # Try to maximize!
    max_path, max_path_score = nil, nil
    node.children.each do |child|
      path, path_score = search(
        child,
        false,
        depth - 1,
        alpha,
        beta
      ).values_at(:path, :heuristic)

      next if path.nil?

      # Is this the best move we've seen so far?
      if max_path_score.nil? || path_score > max_path_score
        max_path, max_path_score = path, max_path_score
      end

      # If considering a branch where the opponent can force a result
      # worse than alpha, forget it, since this is the better option.
      if alpha < path_score
        alpha = path_score

        # Beta was previously set to the worst result the opponent
        # could force. But now we see that if the opponent must never
        # allow us to move to node, because then the maximizer could
        # force a better result.
        return { abandoned: true } if beta < alpha
      end
    end

    return { path: max_path, heuristic: max_path_score }
  else
    # Analogous!
  end
end
```

## Minimum Spanning Tree: Prim's Algorithm

Just stupid Dijkstra's. Doesn't take into account of existing path,
just the cost of the next edge. Same performance characteristics.

Kruskal's Algorithm is apparently more performant in sparse graphs,
though has same time bounds. It uses union-find datastructure which
seems boring.

Borůvka's algorithm is apparently very parallelizable. Could actually
be interesting. **Todo**

## More Graph Problems!

* Johnson's Algorithm allows faster all-pairs shortest path *on sparse
  graphs*.
* Min cut
* Smallest max-edge path
* Largest min-edge path
* K-shortest paths
* Clique detection
* Embedding
* Strongly Connected components
