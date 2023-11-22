First, we can consider a finite game. A finite game consists of a series
of moves, alternating between player, where eventually the sequence of
moves ends. Finite games are finite length strings of moves.

The rules of the game determine which finite sequences of moves are
valid. For instance, some moves are against the rules. Also, some
sequences are not "finished," because the game is not over. We also
specify that if S is a sequence of moves representing a completed game,
then no extension of S is also a completed game: when the game is over,
it is over!

The rules of the game will assign win, draw, or lose to a valid finite
sequence. Sometimes, we will require that there will never be a draw.

A game state S (i.e., move sequence) is "won" for player X if:

(1) The game is over, and X has won,

(2) It is X's turn, and there exists a move $m$ such that (...S, m) is
won for X,

(3) It is Y's turn, and for every move $m$, (...S, m) is won for X.

Let us now consider the infinite. Imagine an _infinite_ sequence of
moves S where every initial substring $S_{1, n}$ represents a valid
sequence of moves. Yet the game never ends. That seems bad!

Now, if at game state S, there is no valid infinite game extension of S,
then either:

(1) Player X can force a win (or draw),
(2) Player Y can force a win (or draw).

This is somewhat obvious.

If there is a valid infinite extension, though, X may need Y's
cooperation simply to end the game. If Y can play a series of moves that
arbitrarily extend the game, then X has not won in the finite sense. If
Y can keep extending, it could try to "blackmail" X to give it a draw,
or even maybe a win. But, since Y cannot force a draw or win, likewise X
can "blackmail" Y. There is a sense in which the game is not
"determined" or "total."

Of course, in some games where infinite extension is possible, the game
can still be considered won. First, a game state S might be won for X in
the conventional sense, even if there is an infinite extension to S, so
long as X can avoid the infinite extension.

We may also disallow trivial extensions. Imagine this game, where the
moves are:

(1) The game can be ended by either player, and X is declared the winner,
(2) The player can pass,
(3) If there have been an odd number of prior consecutive passes, you _must_ pass,
(4) Y starts the game.

Here, even from the start, Y can blackmail. But only in a trivial sense:
Y can never win, or even draw. They can just force the game to go on
endlessly.

We may want to extend our definition "won" to say that this game is won
for X, since Y cannot even extract X's cooperation. We could say: a
state S is won if _every_ finite extension S is also won.

## Infinite Games

Let's consider a slightly different idea. Let's allow infinite sequences
of moves. The game will label all infinite sequences as a win for X or
for Y.

We can modify our idea of an initial finite sequence of moves S
representing a "won" position:

(1) S is "won" if every valid infinite extension of S is labeled a win
for X,

(2) If it is X's turn and X has a move m such that (...S, m) is a won
position for X.

(3) It is Y's turn and for every move m, (...S, m) is a won position for
X.

The question is: are there games where (1) every infinite sequence is
labeled a win for X or a win for Y, but (2) at a position S, the game is
not won for either player?

That means, no matter what a player X does, the opponent has some
response where Y wins in some extensions, but also where X can themself
respond where they will win in some extensions.

Apparently people have studied what it would take for such a game to
exist. People have considered games where (1) you play natural numbers,
and (2) the game length is omega (how could it be more than omega and
have a notion of turns anyway?). The axiom of choice apparently says
that there should exist an indeterminate game like this. But if you take
the "axiom of determinancy", which says no indeterminate game like this
exists, then this would contradict the axiom of choice.

It's an interesting question! It's clearly not obvious whether an
"indeterminate" game like that does exist!

## Computability

The last thing I'd like to mention: you could ask whether a winning
strategy were computable. That is: can there exist a machine X such
that, for a won game state S, as subsequent plays by Y are produced, X
always produces a move m that is a winning move?

## Source

- Phone discussion with Stefano 2023-11-21.
- https://en.wikipedia.org/wiki/Determinacy
