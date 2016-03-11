## Byzantine Generals

Consider a set of loyal and traitor generals. The loyal generals need
to agree on a plan: they must all execute the plan together to
succeed. Traitor generals pose as loyal generals, they will try to get
the loyal generals to do different things. They could also try to get
the generals to adopt a "bad plan."

What is a bad plan? Well, let us say that all loyal generals, if given
the true observations of the other loyal generals, will
deterministically vote for a plan. This is the "good plan".

For the generals to agree on the good plan, they must all get the same
observations from the other generals. The traitors can defect from
this, sending one message to some generals, and a conflicting message
to other generals.

Since traitors can do this, we can't blindly trust a message from a
general; we must use other information from other generals. But this
opens up the possibility that we will disregard and reverse the
message from a *loyal* general, based on other messages from
traitors. And we know tht we must always get all the loyal generals to
use the messages from loyal generals.

Because all loyal generals must ultimately use the same value for
every general (loyal or traitor), and use the *true* value for every
loyal general, we can focus on how any single general could inform the
others of his message. Here there is a concept of a "commanding
general" and "lieutenant generals".

Note that, in turn, each general will have a chance to be commanding
general.

Consider with three generals. If the commanding general is a traitor
and the other two are loyal, then the commander can send different
messages to both lieutenants. One lieutenant could try to inform the
other that the message of the message he received. This would allow
one general to reverse his decision and reach consensus wiht the other
loyal general.

But that strategy would be ripe for abuse if the commanding general
were loyal, but the lieutenant was a traitor. In that case, the
lieutenant could convince the loyal lieutenant that the commander was
a traitor. So we clearly cannot solve the problem with three generals,
one of whom is a traitor.

The next step is to extend this: to show that we can never handle
`>=1/3` traitors. Say that we can tolerate `m` traitors when there are
`2m` honest generals. Then we could use that protcol to solve the 3
general problem: have each general play the role of `m` generals in
the proposed protocol.

Note that it doesn't matter if you only want the generals to
"approximately" agree: say the decision isn't attack/retreat, but to
pick a time to attack. The commanding general is happy if the loyal
lieutenants attack within 10min of the instructured time. But even
that isn't solvable: a solution of this problem would imply a solution
to the exact version. I believe this, since at the end of the day I
think the actual nature of the values being communicated is
irrelevant.

**Solution with >2/3rs Loyal**

Lamport proposes an algorithm `OM(m)`. The base case is `OM(0)`: in
that case, the lieutenants commit to the commander's order.

For `OM(i)`, each lieutenant takes their instruction, and plays
`OM(i-1)`, sending his value. At the end, each lieutenant takes the
value they decided on from `OM(i-1)`, and takes the majority value of
these `n-2` rounds in which they received orders.

**Lemma**

We need to show this will work. Consider if there are more than `2k+m`
generals overall, and `k` traitors. I claim that `OM(m)` will work
*when the commander is loyal*.

This is clearly true for `m=0`; everyone blindly follows a loyal
commander. Let us prove for all `m` by induction. Consider a loyal
lieutenant. They will play `OM(m-1)`. When this happens, there will be
`2k+(m-1)` generals overall, and `k` traitors still. Then by
induction, the loyalists will all correctly receive the lieutenant's
value.

When all is said and done, the loyalist receives `>k+(m-1)` true votes
(from the loyalists). This itself constitutes a majority, so no matter
what the traitor lieutenants send, they cannot confuse the loyalists.

What is this lemma saying? This is recognizing that each round played
with a loyalist as comander reduces the number of loyalists in the
next round by one. That's why, if we want to run `m` rounds, we make
sure to give ourselves an extra `m` loyalists over 2/3s.

**Theorem**

Now consider if there are `>3m` generals, of which `m` are
traitors. By induction, if `m=0`, then it is clear that all loyal
generals reach consensus.

So let's proceed inductively. If the commander is loyal, then we can
use our previous lemma by setting `k=m`. We actually don't need the
induction in that case.

But what if the commander is *not* loyal? We can't use the lemma
directly, then. But consider what the lieutenants do. Any loyal
lieutenant will now lead a group of `>3m-1` generals, of which `m-1`
are traitors. This satisfies the inductive hypothesis, so we know that
the general correctly communicates his value to the other lieutenants.

QED!

**Note on Refusal To Participate**

We assume that messages cannot be lost, or forged. We are only
expecting traitors to confuse us by telling one person something and
another something else.

The traitors could also *withhold* messages. In that case, we can
replace their value with a default value. This works only because we
assume that if we don't hear from a general, they *must* be a traitor.

That's not an entirely realistic model...

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
* Reaching agreement in the presence of faults
