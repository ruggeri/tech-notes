A note about a recurring falacy. Sometimes I here students new to
asymptotic analysis say merge sorting is `O(n)`. They reason
inductively. Say that it takes `O(n)` time to sort half the
array. Then:

1. Sort the left (`O(n)` time)
2. Sort the right (`O(n)` time)
3. Merge the two (`O(n)` time)

So this proves to sort `n` elements takes `O(n)` time! Right?

Well, we know we need a base case. But what does that mean for
asymptotics?

What we need to show is that, for large enough `n`, there exists a `c`
such that the sorting time is `<cn`. Of course, for a given `n_0`,
there exists some `c`. So let's consider our proof again:

1. Sort the left in `<cn/2` time.
2. Sort the right in `<cn/2` time.
3. Merge the two in `O(n)` time.

This won't let you make the conclusion that you can sort `n` elements
in `<cn` time. If steps one and two take `cn/2` time, then it surely
is not possible. In fact, you can prove that it isn't possible, since
for any `c`, we'll be approaching `cn` time as `n` grows, and then
overtaking it.

That's the kind of reasoning needed for this induction and asymptotics
problem.
