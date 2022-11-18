We normally consider polynomial time algorithms to be "tractable." But
why?

Lance Fortnow seems to say: the smallest class that contains linear time
and also is "closed under subroutines" is polynomial time. I'm not
exactly sure what that means? Presumably, it means that you can use a
"tractable" program as a parameter and it should still be tractable?

I'm just marking this down as a question. Exponential time algorithms
will always become slower than polytime algorithms, if given a "big
enough" problem. But why isn't anything bigger than $n^5$ considered
"intractable?" Even if you believe that asymptotic time-complexity is
the right way to measure difficulty, why is polynomial time the right
cutoff between tractability and intractability?
