(Best to read directly from the Nature paper. It is highly readable)

They point out that two traditional approaches are (1) depth-limited,
followed by a valuation function (like minimax with alpha-beta
pruning), (2) Monte Carlo Tree Search, which limits breadth.

## General Background

**Rollouts**

Rollouts are used to evaluate Backgammon. Backgammon is *stochastic*,
you roll the dice and then you move. So what they do is they play the
entire game out, using a simple strategy (called a "policy"). You "roll"
the game out: exploring all the way to the end over many simulations.

To get things to converge better, you should not randomly roll the first
move, but do 1/36th of each trial for 1-1, 1-2, ..., 6-6. But you can't
evenly weight all the way down, because you don't want to do 36^depth
simulations.

You keep track of the win-loss ratio for each initial move. The chosen
move is the one which won the highest percentage of games.

Presumably the result of the rollout method is better than would be
achieved using a simple policy that did no tree search.

AlphaGo does not exactly use rollouts, but it uses MCTS, which is
inspired by rollouts.

**MCTS: Random Style**

How can we use the idea of rollouts in a *deterministic* game like
chess? Instead of using the policy to deterministically choose a move in
response to a random event, we want to non-deterministically choose a
move.

In the most naive form: we could randomly choose a move each time,
playing until the end. At the end of many simulations, we choose the
move that gives us the most wins.

That is a silly idea though. We don't want to make the move that gives
us the highest percentage of wins if our opponent chooses their moves
randomly. We want the move that gives us the most wins if our opponent
chooses their move *well*, and we respond with our own good moves.

Thus, we do need a basic strategy: a policy. Our first improvement is to
choose the opponent's moves using the policy. Our second improvement is
to choose our own moves according to the policy. But that would only
deterministically explore one tree. How do avoid that?

**MCTS: Nondeterminism**

We will use the policy network not to deterministically choose a move,
but to give us a probability distribution on the goodness of moves. This
could be a softmax on a score it gives to each move. We will randomly
choose a move according to this probability distribution.

There are two more bits we want to add: (a) we want to encourage
exploration beyond simply what the policy thinks is a good move, and (b)
if the tree search starts to say a move is good, we should explore it
deeper.

To incorporate (a), the AlphaGo people suggest we choose the move which
is the argmax of a

    P(move) / (1 + NUM_SAMPLES(move))

Where P(move) is the prior probability assigned by the policy network.
This is a deterministic choice function, except eventually we'll explore
the second best move after enough samples of the best move.

We'll see how to incorporate (b) in the next section.

**MCTS: Converging Toward Best Play**

Imagine we are playing through to the end. We record the wins/losses at
each node. Eventually, we want to choose the initial node with the best
win/loss ratio.

Note that this *does not* converge to best play. This chooses the move
that maximizes the probability of winning when moves are chosen with
probability proportionate to the policy valuation.

We need to do (b) from above: we need to increase the probability of a
move based on how good it appears to be from the tree search. We should
choose the argmax of:

    WIN_RATIO + \alpha P(move) / (1 + NUM_SAMPLES(move))

The choice of \alpha will determine how fast we mix information learned
from the tree search with our prior opinion on the goodness of the move.
But eventually the win ratio will dominate. This is the "truer"
information in the sense that it plays out the rest of the game. It will
bias us toward truly better moves. So we should converge to best play.

**MCTS: The Policy Considered**

MCTS will converge (albeit slowly) no matter the policy you use. That's
because over time the WIN_RATIO will dominate. But the better the
policy, the faster the convergence.

**MCTS: Valuation**

You don't really want to play the game all the way out. That would tend
to be slow to converge. Instead, you'd like to quit after a certain
depth, using a valuation function. Instead of WIN_RATIO, you use
AVG_VALUATION.

## AlphaGo Policy Network

**Standard Learning Policy Network**

First, they train a policy network to predict moves played by great
chess players is good games. It apparently uses just CNN layers (13
layers). They choose a random state action pair from an expert game,
feed the state into the network, and the network tries to pick the
expert chosen action. This is basic SGD.

The SL Policy Network picks the expert move as the most probable 55% of
the time.

**Fast Policy Network**

They trained a very fast policy network which used just a subset of
features. This had only a 24% accuracy, but took 2 microseconds to
evaluate versus 3 milliseconds for the SL policy network.

**Reinforcement Learning Policy Network**

They next play the policy network against itself. Actually, they
randomly choose a previous iteration of itself to play against, so that
it does not overtrain on itself.

They don't do any fancy attribution. As far as I can tell they just do
gradient ascent, reinforcing all action choices in a winning game, and
punishing all action choices in a losing game.

The RL network eventually beat the SL network 80% of the time. It won
85% of the games against Pachi, the best open source Go program.

Reminder: both the SL and RL policy networks do no tree search at all.

## AlphaGo Valuation Network

They train a network to predict whether perfect play would win from a
given position. They don't know perfect play, so we'll settle for the
probability that the RL policy, if played against itself, would win.
They approximate this probability function with a neural network.

To train, they generated 30MM self-play games. They trained by using
just a single position in each game. They didn't want to train on
multiple positions in each game, since game states are highly
correlated.

**Valuation Network Evaluation**

They compare the valuation network to four other ways of valuing:

* Full rollouts with a random policy (very bad),
* Full rollouts with a "fast" policy network (not quite as good as the
  value network),
* Full rollouts with the SL policy network (better),
* Full rollouts with the RL policy network (better).

We expect rollouts with the SL/RL policy networks to give a better idea
of who wins. They're doing actual search! But it's impressive that the
valuation network is better than rollouts with a "fast" policy network.

They note that the valuation network was almost as good as the RL policy
rollouts, but with 15,000x less computation.

## MCTS In AlphaGo

As discussed, they use the policy network to assign a prior probability
on moves to explore. They adjust divide this by `1 + N(move)` to
encourage exploration rather than just exploitation.

However, we want to learn from our simulation experience also! So they
also incorporate `Q(move)`. This is the average valuation of trials
through this node.

The leaf node valuation is a weighted average of (1) the valuation
network result, and (2) a single random rollout using the fast policy.
They used a 50/50 mixing.

Note: the prior probability `P(move)` is based on the *SL* policy
network. The SL network was the one trained on expert moves. Since
experts vary in style, the SL network was less biased toward looking at
just the one move the RL network feels sure is the very best. That is:
the SL network encourages us to search a wider *beam* of good moves.

The RL policy was still useful in training the valuation network,
though!

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
