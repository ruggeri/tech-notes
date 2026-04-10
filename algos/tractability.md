We normally consider polynomial time algorithms to be "tractable." But
why?

Sure, an exponential time algorithm will get slower at a faster rater
than any polynomial time algorithm, as problem size increases. But
$O(n^5)$ also gets slower faster than an $O(n^4)$ time algorithm, too!
So cutting off tractability for this reason alone at exponential time is
no more justified than cutting off earlier at say $O(n^5)$.

In cryptography, you sometimes rely on a process that you can perform in
polynomial time, but that an adversary can only reverse in exponential
time. That allows you to achieve any securer/attacker effort ratio
desired. But that is also true if encryption takes $O(n^4)$ time and attack takes $O(n^5)$ time...

For a practitioner, even $O(n^2)$ may be intractable. It's not like
polynomial time means _fast_. Some exponential time algorithms can be
faster in practice (for small enough problem sizes). Asymptotic analysis
is useful for suggesting future system bottlenecks, but those are
frequently polynomial time processes with exponents $k>=2$.

The notion of "tractable" is more of a theoretician's idea. I believe it
comes down to decision problems and P and NP. An NP problem _must_ be
solvable in $O(2^n)$ time because there is a witness of length $n$
that can be checked in polynomial time.

As a practical matter, a lot of important problems appear to either
require an exponential time solution (basically involving checking
practically all combinations), or ar solvable in polynomial time with a
low exponent. Probably if there were more intermediately difficult
problems, we'd develop a more nuanced notion of tractability?

Still, I'm not sure if it really is justified to call polynomial
algorithms tractable. Certainly not from an engineering perspective. Why
a theoretician makes this distinction is still a bit unclear to me (but
see below about simulation).

# Sources

- Lance Fortnow: https://blog.computationalcomplexity.org/2002/10/complexity-class-of-week-p.html
  - Lance Fortnow seems to say: the smallest class that contains linear
    time and also is "closed under subroutines" is polynomial time. I'm
    not exactly sure what that means? Presumably, it means that you can
    use a "tractable" program as a parameter and it should still be
    tractable?
  - Fortnow suggests that polynomial time is fairly model independent.
  - Polynomial time I believe allows machine simulation, which takes
    polynomial time overhead. Probably a universal turing machine can in
    polytime simulate all TMs that run in polytime.
  - The ability for a UTM to simulate any other polytime TM with at most
    polytime overhead is what makes polytime the natural choice for
    "tractable" for theoreticians. You don't lose tractability through
    simulation on a UTM.
