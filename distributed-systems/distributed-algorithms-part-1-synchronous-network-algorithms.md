## Ch2: Synchronous Network Model

* Each round we can send one message on each link.
* We will consider two failure types:
    * Process failure
        * Stop failure.
        * Byzantine failure: produces message in an arbitrary way.
    * Link failure
        * Link can lose message.
        * Sender doesn't know that link lost the message.
        * Typically won't allow corruption, as we can
          cryptographically sign.
* Will measure time complexity, which is the number of rounds until
  completion.
    * But sometimes communication complexity matters, because all
      messages may be sent on a shared, contested medium.

## Ch3: Leader Election in a Sync Ring

* If all processes are truly the same, then there is no way to elect a
  leader. Because they're identical! So we break the symmetry by
  introducing a UID.
    * Note that this could be added if we have access to an RNG.
* One appraoch is to pass around the largest identifier. If you get
  your own UID back, you have the largest UID, and can be elected
  leader.
    * They do this in `n` rounds, with `n**2` messages, by having
      everyone simultaneously send their token. But only tokens get
      passed onward if they are better.
    * This is called *LCR*, for Le Lann, Chang, and Roberts.
* It proposes a zany algorithm (`TimeSlice`):
    * Assuming there are `n` nodes, it says that UID `v` can only
      circulate during times `vn...(v+1)n`. That gives it enough time
      to go around.
    * This uses `n` messages, but takes an unbounded amount of time!
* It also proposes an algorithm that passes `O(n log n)` messages, and
  is still `O(n)` rounds.
    * I'm not that interested.
    * Requires being able to pass messages backward and forward.
* NB: the obvious algorithm of someone starts passing their ID around
  the circle will not work, because that would make that one machine
  unique.
    * Normally someone can kick off an election, and then they could
      be the initiator.
    * This chapter feels very synthetic...
* Has some boring proofs...

## Ch4: Algorithms in General Sync Networks

* FloodMax:
    * Each round, just say the best UID you've seen before on all
      channels.
    * If we have a network of diameter `d`, this takes `d` rounds. But
      it has very high communication complexity.
    * We can improve this by only sending a new best UID when we learn
      of one.
* SynchBFS
    * Each node sends message to all neighbors, which mark themselves.
    * Could even send a confirmation up all the way back to the
      parent.
    * Communication complexity is proportionate to the number of
      edges, overall time is proportionate to diameter.
* SynchBFS can be used to:
    * Broadcast a message.
    * Compute a global value from distributed inputs.
    * Example: Elect a leader. Calculate leader with highest UID.
    * Caculate diameter. Can calculate height of the BFS tree,
      broadcast it to everyone. Everyone does this in parallel.
* Bellman-Ford
    * Best weighted path.
    * There's a starting node. It has `dist=0`, `parent=nil`. Each
      round (really each time you get a new dist), you push to all
      neighbors your `dist`; if `dist+link_cost` is better, then you
      update `parent` and `dist`.
    * Bascially, it's like a stupid version of Dijkstra, excep it
      expands everyone simultaneously.
    * This is going to take `O(n-1)` rounds. It's not `O(diam)`,
      because we're talking about the min weighted path, which may
      have more than `diam` links!
* MST
    * They do this thing where, in parallel, you slowly connect
      trees.
    * At each level, you have to choose the edge to include.
    * This is going to take `log(V)` steps like this. Each step takes
      up to `O(V)` time to compute the best edge to add and to
      communicate this.
    * These notes are kinda lazy, but I'm not super interested, I
      suppose.

## Ch5: Distributed Consensus with Link Failures

* Consider if we have many generals, who all must agree whether to
  attack, or not to.
    * Without failures, we know that we can send messages out BFS
      style so that everyone knows the state of everyone else.
    * Now we'll talk about losing messages, where we won't know
      whether our message was received.
* Corresponds to distributed commit: we want to commit the transaction
  if everyone agrees that they can do so.
* To avoid a trivial solution, we require that if everyone wants to
  commit, we *must* commit, while if everyone wants to abort we *must*
  abort.
    * Important: if a mesasge is lost, we are allowed to abort, even
      if everyone wants to commit. Otherwise we could never get
      everyone to commit if the adversary blocked all messages (since
      we wouldn't know what anyone was saying).
    * Lynch is formulating a very weak version of the problem: we
      always have to agree at the end, but if one person wants to
      commit and another abort, either consensus is okay.
    * Other natural formulations would talk about a *majority*, or
      require that we can't commit unless everyone wants to.
    * Our problem is less specified, and it still won't be solvable.
* When messages can be lost without our knowledge, no algorithm is
  guaranteed to solve the problem.
    * Consider a solution to the problem where there was no message
      loss. We eventually stop.
    * But what if we lost the last message, and all subsequent ones?
    * Then one machine thinks it's over, but the other machine keeps
      trying to tell it that the expected message has not been
      received.
* The problem basically is:
    * Someone stops after sending their last message. What if this
      message is the one that is lost? They'll never know.
* A randomized algorithm:
    * Everyone keeps track of "information level"; the information
      level increases by one only when we've heard from everyone in
      the previous level.
    * We're going to run for a max of `r` rounds. If we get a message
      saying someone wants to abort, we'll definitely abort. We'll
      only commit if everyone wants to commit.
    * One other thing, we'll pick a threshold information level in
      `(1..r)`. We'll only commit if our information level exceeds
      this.
    * BTW, process one is going to pick the level for global use.
* I claim this has an error rate of `1/r` for any adversarial strategy
  of disrupting messages. (Adversary can't read messages).
    * Basically, each message the adversary kills reduces the max
      acheivable information level by one.
    * Best play is to guess `threshold`, let the first `threshold-1`
      rounds go through fine, then kill all messages from one process
      afterward.
* This is basically the best you can do!

## Distributed Consensus with Process Failures
