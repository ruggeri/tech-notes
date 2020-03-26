I did a lot of junk trying to prove Euler's theorem $x^{\vphif(n)} = 1$.
This is an easy corollary of Lagrange's theorem. The easiest way to
prove Lagrange is via cosets.

What I was *trying* to do was show that every subgroup of order $\prod
p_i^{k_i}$ factors into subgroups of order $p_i^{k_i}$. I called these
``chains.'' I then hoped to show that *those* subgroups are all cyclic,
and thus the overall group is cyclically generated.

However, this is *not* true. A subgroup of order $p_i^{k_i}$ *may or may
not* be cyclic. There are as many subgroups of order $p_i^{k_i}$ as
there are partitions of $p_i^{k_i}$. This is the Fundamental Theorem of
Finite Abelian Groups.

Which leads us to ask: can that happen when we are specifically working
with $\Zmodx{p^k}$? (rather than just an arbitrary group of size $p^k$).

The answer is: sometimes $\Zmodx{p^k}$ is cyclic. In this case, we say
that there is a \define{primitive root of unity modulo $p^k$}.
Basically: a generator.

Other times $\Zmodx{p^k}$ is \emph{not} cyclic. This is precisely when
the \define{Carmichael function} takes on value less than $\phi(p^k)$.
It is precisely when a primitive root does not exist.

Euler's primitive root theorem tells us exactly when a primitive root
exists. Carmichael's theorem tells us a recurrence relationship defining
$\lambda$.

These ideas are properly developed in my Abstract Algebra notes (where I
prove the Fundamental Theorem Of Finite Abelian Groups), and in my RSA
notes (where I explore formulae for the $\vphi, \lambda$ functions).
