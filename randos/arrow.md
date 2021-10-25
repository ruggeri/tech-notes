## Arrow's Impossibility Theorem

You have individual preferences over alternatives, and you want to
aggregate to community preferences. The preferences are represented as a
complete, transitive relationship: a linear ordering. The preference is
ordinal; there is no cardinal valuation of the alternatives.

The aggregation method will take submitted individual preference
relations, and produce a community preference relation. We want three
properties:

- There is _no dictator_: the function should not simply copy the input of
  one participant as the output.
  - A weaker version could be: given the other participants' preference
    inputs, the function restricted to a last participant should no
    longer be _surjective_. That is: there should be some ranking that
    is no longer possible, even if this final participant knew the other
    votes and would want to try to force this preference.
- If every voter submits a ballot ranking alternative alpha over
  alternative beta, then alpha should be ranked in the community
  preferences higher than beta.
  - This is called _Pareto efficiency_. That is: there should not be a
    change that makes everyone happier and no one less happy.
- Independence of Irrelevant Alternative (IIA)
  - This says that the aggregation method should hold constant the
    ranking amongst a subset of the alternatives S, regardless of any
    change to the rankings of alternatives _outside_ S.

Arrow's theorem says that you cannot achieve all three simultaneously.

TODO: how does _monotonicity_ relate to Pareto and IIA?

## Gibbard-Satterthwaite Theorem

Arrow tries to aggregate individual preferences into a community's
preferences. What if it were simpler: we want simply to _select_ a
_single_ alternative to be the community _choice_.

Then Gibbard-Satterthwaite says that one of the following must always
hold:

1. There is a dictator, OR
2. The choice mechanism intrinsically restricts the outcome to one of
   two results, OR
3. The choice mechanism is susceptible to _tactical voting_. That is: a
   sincere ballot may not be a voter's best play.

The theorem is based on ordinal voting. The last property is a form of
_monotonicity_.

This theorem seems mostly like a corollary of Arrow, and I believe it
can be proved that way. However, one might have hoped that the simpler
task of merely _selecting a single alternative_ might be more achievable
than the grander task of devising a community preference relationship.

## Gibbard's Theorem

Gibbard's Theorem generalizes to deterministic processes which select an
alternative, where each player controls an input, but where the input is
not constrained to be a preference relationship.

For instance, cardinal utilities could be submitted.

The desiderata are analogous to what has come before:

1. No dictatorship,
2. Allows more than two possible possible outcomes,
3. No strategic voting: there should be a single input that best defends
   the player's preferences, no matter the inputs of the other players.

## Sen's Paradox

Another "paradox" of social choice. Here, the inputs are preference
relations, and the output is a _set_ of alternatives that are selected.

Unrestrictedness: invariant under relabeling of participants and
alternatives (I think).

Pareto optimality: If everyone prefers X to Y, then Y must not be
amongst the selections.

Liberalism: every player has a pair of options $x_i, y_i$ on which they
are _decisive_. That is: if they prefer $x_i$ to $y_i$, then $x_i$ must
be selected.

TODO: https://en.wikipedia.org/wiki/Liberal_paradox#The_theorem

I haven't got time to go deeper into this right now. I mention this
simply because I came across it in Anarchy, State, Utopia.
