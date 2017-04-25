(Best to read directly from the Nature paper. It is highly readable)

They point out that two traditional approaches are (1) depth-limited,
followed by a valuation function (like minimax with alpha-beta
pruning), (2) Monte Carlo Tree Search, which limits breadth.

**MCTS**

Let's talk MCTS for a moment. MCTS is kinda like rollouts. In a
rollout, you play the same position many times with a very simple
strategy. From this, you can determine the "equity" of the position:
its value. Empirically, this can be a good estimator of the estimator
of the value.

Rollouts are used in backgammon, which are stochastic, where even if
you know a generally pretty good strategy, the truly correct action
will depend a lot on chance.

MCTS is a tree search method. You evaluate moves "semi-randomly". You
do a move ordering type thing. You explore a move, each time picking
what looks like the best move until you get to the bottom.

That tells you what would happen for one game, but maybe one of what
you thought was the "best-move" was not? That's the whole point.

So what you do is start again from the root. You keep track of how
many times you've tried each next move, and also how many wins you had
with that move. Your heuristic should assign each move a *prior
probability*. This determines how promising it is. Now, you combine
all this information.

That is, you favor (1) moves that you initially thought were good, (2)
which resulted in you winning often, and (3) which haven't explored
much yet.

Note: "pure" Monte Carlo Tree Search would just play random moves and
pick the one that resulted in the most wins. That would be horrible at
chess.

Another note: you'll have to balance exploration and exploitation.

MCTS, because it focuses on the most promising trees, is preferred in
games with a high branching factor.

**Policy Network**

The choice of moves to make for MCTS is very important to its
efficacy. So the typical approach is to train a network to predict
expert player moves. The AlphaGo authors claim that earlier work used
shallow nets or linear valuation functions. They use a 13-layer
convolutional neural network.

**Four Networks**

They train a supervised network to predict expert player moves. They
also train a version which is very fast and can be used for rollouts.

They then use the supervised network to initialize a reinforcement
learner, that will play against itself. It will improve the network to
improve its play against itself. This adjust the network away from
predicting an expert's moves to playing good moves to win the game.

Last, they trained a valuation network that predicts the winner of
games played by reinforcement learned network against itself.

## SL Network and Fast Rollout Policy Network

Trained on state-action pairs of expert games. Job is to predict the
action from the state. 13 layers of CNN, alternating convolution with
rectifier nonlinearity. Predicts expert moves at 55% accuracy. They
noted that small improvements in accuracy greatly improved AlphaGo's
win rate.

They also trained a fast rollout policy network the same way, except
it was a much simpler network. I believe it was just a linear
softmax. This acheieved 25% accuracy, but took 2 microseconds rather
than 3 milliseconds to evaluate.

## Reinforcement Learning

They did policy gradient reinforcement learning to improve the SL
policy network. It started out initialized to SL, but then updated
itself as it played prior versions of itself. It played a sample of
older versions of itself so that it didn't become just good at beating
itself.

They found the RL network beat the original SL network 80% of the
time. They found that it won 85% of games against the strongest
open-source Go program. NOTE: the RL network *does no search at
all*. It's just playing based on what it thinks experts would do.

## Valuation Network

They learn a network to value the probability of winning when the RL
program plays itself. Their idea is that a good valuation function
would assume perfect play, and the RL network is the best they have.

Their valuation network has similar structure to the policy networks,
but only outputs a single number: the value.

Important note: if they fed a whole game to this network to train on,
it will just memorize the outcome and figure out that certain boards
are from the same game. So instead they played 30MM games and just
used one move from each.

To compare. A uniform random rollout policy was very bad at predicting
the winner of the game. Their fast rollout policy trained from expert
moves was quite good. Better yet was doing rollout with a policy of
the larger SL and RL networks.

The value network did almost as good as the SL and RL rollout
policies, and especially in the middle of games was *much* better than
doing the fast rollouts.

Running the valuation network was 15k times faster than the RL policy
network rollout, but nearly as good.

## Searching with the Policy and Value Networks

This describes MCTS a bit. They want to argmaximize:

    Q(s, a) + P(s, a)/(1 + N(s, a))

The `Q` is the value based on experience. `P` is the prior probability
of that `(s, a)`. `(1 + N(s, a))` is how many times we have visited
this already.

Now, to evaluate `P`, they use their SL network trained to predict
expert moves.

Leaf nodes are evaluated first with the valuation network, and also
with a rollout using the fast rollout policy. They are then mixed
together linearly.

Now, everyone in the tree on this path has `N(s, a)` incremented. We
also update `Q(s, a)` by adding the valuation of the newly tried leaf
and dividing by `N(s, a)`.

Note: we switch to the fast policy network when we evaluate a leaf. We
use the slower, more thoughtful network to make decisions down to the
leaf.

They note that the SL network does better than the RL network at
choosing moves to try out. They note that there is perhaps a wider
beam of moves that humans will consider. On the other hand, they found
the value function derived from the RL network was better than the one
derived from SL.

## Relative Value of Networks

Using just rollouts got an ELO of 1500. Just the policy network or the
value network did not significantly improve this.

Rollouts plus the value network or the combo of the value and policy
networks were much better at about ELO 2250. Using rollouts and the
policy network got to ELO 2500.

Using all three got to ELO 2500.

Their conclusion is that the rollout and value networks are
complementary.

## Hardware

Because evaluation using the CNNs were slow, they did this on like 40
CPUs and 8 GPUs.

They also did a distributed AlphaGo on 128 GPUs. This beat the
single-machine vesrion 77% of the time. It never lost to any other
AI. The single-machine AlphaGo won 494 of 495 games against other AIs.

## Conclusions

They note they evaluated many thousand times fewer positions than Deep
Blue.
