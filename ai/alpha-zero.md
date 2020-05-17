AlphaGo Zero is a version of AlphaGo that was trained just with RL.
AlphaZero is a non-Go specific version, also trained entirely via RL,
and that ended up with better performance still.

They use the same general architecture. They have a network that outputs
a policy vector, and that also outputs a valuation.

They note that when training with MCTS, MCTS will slowly evolve
probability distributions over possible moves. We talked about how shift
from a prior distribution to a distribution based on experience of
playing out games. The RL training does gradient ascent to make the
prior distribution on moves look more like the distribution found via
MCTS.

That is: we don't just play a game once to the end using the policy, and
reinforce moves based on whether we won or lost. The RL training
involves MCTS, guided by the current policy, and then we update the
policy to look more like the MCTS distributions.

Another big difference: while AlphaGo does a rollout with a fast policy,
AlphaGo Zero uses only its valuation function.

Note: I don't think I was clear on when MCTS terminates. I think each
iteration of MCTS expands one leaf node. To evaluate the newly expanded
node, AlphaGo (1) runs the valuation network, and (2) runs a single fast
rollout. It mixes this information to update ancestor node's valuation.

## Other Differences

It looks like they used CNN architecture for AlphaGo, but a residual
network for AlphaGo Zero. Just switching to residual (and using the same
training regime as AlphaGo) gave a 600 point ELO boost.

AlphaGo Zero combined policy and valuation into a single network. They
trained with a single loss function (MSE on the valuation, cross-entropy
on the policy). This is instead of training two separate networks for
AlphaGo. They report that policy accuracy was reduced, but valuation was
better. This boosted performance by about 600 points ELO again.

They note that, in a sense, AlphaGo Zero was not constrained by prior
human theories of Go.

## Other Notes

When doing MCTS, they do 1600 simulations to select each move. That
takes 400ms.

They note that they don't recompute MCTS afresh every time. They'll warm
the next move's MCTS by using the corresponding subtree statistics from
the prior move's MCTS.

https://discovery.ucl.ac.uk/id/eprint/10045895/1/agz_unformatted_nature.pdf
