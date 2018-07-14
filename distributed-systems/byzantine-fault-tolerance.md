## Byzantine Generals Setup

Consider a set of loyal and traitor generals. The loyal generals need
to agree on a plan: they must all execute the plan together to
succeed. Traitor generals pose as loyal generals, they will try to get
the loyal generals to do different things. They could also try to get
the generals to adopt a "bad plan."

In the algorithm, every general will submit a value. We require:

1. If a loyal general `i` votes a value `v_i`, then every loyal
   general must make their decision using `v_i`.
2. For a nonloyal general, it doesn't matter what value the loyal
   generals use to make their decision. However, *every general must
   use the same value.*

We may now recast the problem: one "commanding general" tries to
communicate their value to all the other generals (called
lieutenants). All lieutenants must agree on the value, and if the
commander is loyal, they must agree on the *right* value.

If we can solve this problem, we can just iterate through each of the
generals to get everyone to communicate their submitted value.

Of course, the traitors can defect from the algorithm however they
want.

## Impossible To Solve With 3 Generals, 1 Traitor (without signatures)

Consider a loyal lieutenant. They get a message `v1` from the
commander, but a conflict message `v2` from their fellow lieutenant.

Which one is lying? If the commander is loyal, then they *must* vote
`v1`. Otherwise, one of the loyal lieutenants must change their vote
(to come into sync with each other). WLOG, let's say that lieutenant 1
must change their vote to `v2`, iff the commander is a traitor.

With just one round of communication, a loyal lieutenant cannot
distinguish the two possibilities. But the problem is that future
communication rounds can't help.

## Cannot Solve With >=1/3 Traitors

It's a proof by contradiction. Assume we could solve the problem with
`3m` generals even when `m` of them were disloyal. We'll show how that
would imply a solution to the three generals problem (which we see is
unsolvable).

If we can do it, have each general simulate `m` participants. The
general simulates `m-1` lieutenants and a simulated general, too.

We could then just run the algorithm. The loyal lieutenant would have
all their simulated lieutenants arrive at the same value, which the
loyal lieutenant knows is the correct value.

## Approximate Solution Not Possible

Note that it doesn't matter if you only want the generals to
"approximately" agree: say the decision isn't attack/retreat, but to
pick a time to attack. The commanding general is happy if the loyal
lieutenants attack within 10min of the instructured time. But even
that isn't solvable: a solution of this problem would imply a solution
to the exact version. I believe this, since at the end of the day I
think the actual nature of the values being communicated is
irrelevant.

**TODO**: I kind of skipped over this in the paper.

## Solution In Sync Model; 2/3+ Loyal

First, Lamport is specific about the model:

1. Every sent message is delivered correctly.
    * Redundancy checks?
2. Receiver knows who sent it.
    * Cryptography?
3. Absence of a message can be detected.
    * This isn't true. Can have async model.
    * In paper they assume that we can detect withheld messages, in
      which case we know it's a traitor, and we substitute a default
      value.

Basically, this is the *synchronous model*.

Lamport proposes an algorithm `OM(m)`. The base case is `OM(0)`: in
that case, the lieutenants commit to the commander's order.

For `OM(i)`, each lieutenant takes their instruction, and plays
`OM(i-1)`, sending their value. At the end, each lieutenant takes the
value they decided on from `OM(i-1)`, and takes the majority value of
these `n-2` rounds in which they received orders.

Lamport claims `OM(m)` works if there are `m` traitors and `3m+1`
participants.

## Proof Works For m=0, n>=1

Clearly works, as there are no traitors. Everyone can be trusted.

## Proof For m=1, n>=4

If the commander is the liar, everyone else is honest. They all see
the commander's messages truly, and act in concert.

More dangerous is if the commander is *not* the liar. Then here each
honest lieutenant has the `v` value they were sent by the commander, a
`v` vote from the other honest partner, and an `x` vote from the
liar. But the liar is in minority, and so loses.

## Proof for m=2, n>=7

**Commander is liar**

If the commander is the liar, then the subalgorithm works for `m=1`,
`n=6`. The lying commander may send different msgs to the honest
lieutenants, but the subalgorithm works properly.

Likewise, the lying lieutenant, when they play as commander, cannot
fool anyone. Thus all honest players see the same set of messages.

Note: the possibility of this lying lieutenant is the reason we must
play a nested version of the algorithm. In fact, you must always play
a subround if (1) the commander is a liar and (2) a lieutenant is a
liar. Because the commander can make it almost a totally split vote,
and the lieutenant then pushes things over the edge.

Also note: to catch the lying lieutenant, we only need 3 others to
catch them in the subalgorithm. So if we know the commander is the
liar, we really only need five participants.

**Commander tells truth**

More dangerous: the commander is a truth teller. That leaves two liars
acting as lieutenants.

Each lieutenant plays the subalgorithm, as we've already seen is
required in case the commander is the liar. In this scenario where the
commander is honest, it would have been preferrable *not* to play the
subalgorithm, because it offers a chance for the liars to subvert the
honest lieutenants' attempts to forward the message they received.

So let's consider each honest lieutenant playing the
subalgorithm. Everyone will accept the majority vote of what people
say about this lieutenant. Which means: we need at least *3* votes to
counter the two liars. That's what brings us to needing a total of
seven participants:

* The commander,
* The lieutenant, acting as subcommander,
* Three honest lieutenants,
* Two liars.

As noted previously, the traitor lieutenants *will* be able to fool
the others about the message they received from the commander. But it
won't matter, because they are in the minority. They can only succeed
by confusing people about the honest lieutents' messages.

## Induction

If you must tolerate `m` failures, that means:

* You must keep playing subrounds until there is only one liar
  left. You can never trust the base-case algorithm if both the
  commander and a lieutenant could both be traitors.
* If, at any of the rounds before the basecase round you have a
  traitor commander, everything will be fine. The subalgorithm will
  work, because the number of traitors has dropped by one.
* Most troublesome case is if we recurse `m` rounds with all honest
  participants. Then, to work, we must have `m+1` honest lieutenants
  left, in addition to the `m` dishonest ones.

And thus we need `3m+1`!

## With Signed Messages

The problem is a lot easier if we have digital signatures. This
restricts the ability of generals to lie to each other; their lies
will be detected!

In that case, the protocol is as simple as signing your value and
sending it to everyone. Then everyone signs your message and sends it
to everyone else.

Any traitor who lies in the first stage will be caught out. A traitor
can't lie in the second stage, because that would require forging
signatures to other letters.

Therefore, to tolerate `m` failures, you need the minimum `m+2`
participants. The problem is vacuous (lol, their word) with just one
honest participant (`m+1`).

## Missing Communication Paths

The paper goes on to demonstrate how you might modify the algorithm to
acheive consensus when communication paths are missing.

If the only communication path from the commander to a loyal general
goes through traitors, then you are fucked. But so long as the graph
of loyal generals is connected, you can do the signed message
algorithm.

It's harder if you can't sign messages. In that case, you can still
run a version of the algorithm. But it requires this: that for every
vertex, there is a set of neighbors of size `3m` such that from every
neighbor, there is an independent path from each of these neighbors to
any other node in the graph. The graph is said to be `3m`-regular.

That turns out to be a *very* high degree of connectivity. E.g., in a
graph with `3m+1` nodes, `3m`-regular implies a complete graph (no
missing paths).

**Note**: I didn't review this part because it wasn't important to
me...

## Summary

Note that communication line failures look like processors lying
(timeout and use a default value). So the algorithm tolerates up to
`m` communication lines failing. A note: if multiple lines from one
processor fail, that only counts as one failure.

The assumption that impersonation not occur implies that fixed,
dedicated lines of communication are used. Or signatures, but then
everything is so much easier...

We need to be able to timeout waiting for a message, which means we
need to have some level of clock synchronization. But that's almost as
hard as Byzantine Generals, they say. They say they'll talk about that
in a future paper...

## References

* The Byzantine generals problem
    * https://www.microsoft.com/en-us/research/uploads/prod/2016/12/The-Byzantine-Generals-Problem.pdf
* Reaching agreement in the presence of faults
