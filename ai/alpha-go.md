## Policy Network

* Appears to have first trained a neural net to try to emulate play of
  historical games.
* Then trained against itself with reinforcement learning.
* This is the "policy" network. It alone is good enough to beat almost
  all extant Go programs, without any search!

## Value Network

* Using policy network and reinforcement learning, trained a neural
  net to value a position.
* That is, predict how likely it was to lead to a win.

## Monte-Carlo Search

* Randomly sample moves that the policy network suggests.
* The value network stops us from having to go all the way down.
