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
* One approach is to pass around the largest identifier. If you get
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
        * I think, if you want all the processes to be symmetric, you
          can have them radnomly generate IDs to see who will get to
          be process 1.
        * But I think maybe that doesn't matter, since we're assuming
          the size of the network is known.
* I claim this has an error rate of `1/r` for any adversarial strategy
  of disrupting messages. (Adversary can't read messages).
    * Basically, each message the adversary kills reduces the max
      acheivable information level by one.
    * Best play is to guess `threshold`, let the first `threshold-1`
      rounds go through fine, then kill all messages from one process
      afterward.
* This is basically the best you can do!

## Ch6: Distributed Consensus with Process Failures

**Model**

* Here, we will assume messages cannot be lost.
* But failures can be stopping failures or Byzantine.
* For stop failure, we'll allow a stop to happen in the middle of a
  round; some messages are sent, but not at all.
* Again, we need that if all (correct) processes start with the same
  value, we need everyone to agree to that value. Else, they can agree
  to anything.
    * All nonfaulty processes must agree to the same value.
    * That's *different* than the communication failure mode, where
      once we detected link failure we could agree to an arbitrary
      value.
* She notes that algorithms typically are specified to work for a
  maximum number of errors. But she notes that this is unfortunate,
  and that a probabilistic algorithm that gives some probability of
  correctness as a function of an underlying error rate might have
  been more realistic.
    * She says this because specifying an upper bound on failures
      implies that incremental failures are *negatively correlated*.
    * Each failure makes it less likely that a second failure will
      occur, else the threshold could be breeched.
    * That, of course, is unlikely.

**FloodSet: Solving for Stop Failures**

* Broadcast all seen values each step.
* Collect all seen values in a set.
* After `f+1` steps, choose the unique value, or, if the set has
  multiple values, a default value.
    * Clearly works easily for commit/abort.
* Why `f+1`? Because we want to allow `f` failures; if we only
  went one round, a process could send some messages but not others.
* By the `f+1` round we know that everyone has failed, so that we
  are free and clear to collect all values.
* Else what could happen: P1 only manages to send its value to P2, but
  no one else. Next round, P2 tries to send P1's value to everyone (no
  one else has it), but it only gets to P3...
* Probably could use any decision rule, by collecting set of
  `(Proc ID, Vote)`.
* Alternatively, we could reduce communication complexity by just
  sending our first message, and whether we saw a second,
  different vote. In that case, communication complexity is
  `O(2n**2)`. That works for certain binary decision rules, like
  commit/abort.

**Information Gathering Tree**

* Root (named `\lambda`) is filled in with our initial value.
    * The next level has `n` nodes, each called `1,..,n`. At each
      node, we store the value that Pi told us.
    * Next level has `n**2` nodes, labeled `(1, 1),...(1,
      n),...,(n,n)`.
        * At node `(i, j)` we store the value that Pj told me that Pi
          told him.
        * We do not bother to store `i=j`. In fact, in deeper trees,
          each coordinate should be unique.
        * Et cetera.
* At each stage, every node communicates the entire fringe.
    * That's an exponential communication complexity; still `n**2`
      messages in `r` rounds, but think of the bits! That's `O(n**r)`
      bits each round!
* We can run FloodSet like this.
    * Some people will get different intermediate parts, but
      eventually everyone will have the same fringe.
    * Though some parts of it will be empty, of course.
    * But it's not really an improvement, because of the ridiculous
      communication. Right now it is a curiosity.
* I think this tree is basically replicating *all* the information
  that's been communicated.
    * We're basically letting everyone see everything we know.

**Authenticated Byzantine Failure**

* A version of Byzantine failures with *signatures*. You can lie, but
  you can't forge a message.
* What you do is you require each machine to sign each message it
  sends. You drop messages that aren't correctly signed.
* In fact, you'll use a chain of signatures. This way, a faulty
  process can lie about its own value, but cannot lie about what
  one process told another.
* For instance, if `f=1`, you'll need two rounds. In the first,
  everyone signs their vote and sends it to everyone else.
    * In the second round, everyone signs and sends what they heard
      the first round.
    * No one can forge what they heard from someone else.
* A faulty processor can try to break coordination by sending some
  machines a message but not others (simulate a stop failure).
    * If `f=1`, it doesn't matter if the faulty processor simulates a
      stop failure in the second round.
    * That's because if it lied in the first round, the other machines
      will catch it.
* Consider `f=2`.
    * One machine signs two messages to another, handing it a lie.
        * Basically, traitors can share private keys, or just be one
          party.
    * In round one, the traitors tell no lies.
    * In round two, T1 lies about what T2 told him. This should get T2
      invalidated.
    * The trick is, T1 is going to tell *some* of the honest machines,
      but not others!
    * That's why we need to run `f+1` rounds!

**Byzantine Failure With Three Nodes**

* Say that `n=3`, but `f=1`.
    * Now if both honest processes want to commit, we're supposed to
      commit, even though a liar might want us to abort.
    * If we insist that 100% of people must vote to commit for a
      commit to go through, one dishonest actor is going to be able to
      stop us from committing, *without even lying*.
    * So that we may tolerate failures, we may say that if >X% want
      to commit, we will commit.
    * Note that the lower we set X, the harder it is to get people to
      agree to *abort*.
    * So we might as well set `X=50%`, for symmetry's sake.
    * Without lying, there's no question that we'll have consensus,
      just a question of whether it's the *right* consensus.
* Note that this is not a **distributed commit solution**! Some people
  can vote to abort, but we still won't abort.
    * Right now we're building something for majority decision making,
      where everyone is made jointly aware of the majority's decision.
* Great, but then you have liars, people who say one thing to one
  node, and something else to another!
    * Liars won't be able to stop a unanimous decision (if there are
      <50% liars).
    * Their goal is to, in smaller numbers, disrupt a split decision.
    * The goal of liars is to make some people think that a majority
      thinks ABORT, while making others think a majority favors
      COMMIT.
    * The greater the consensus amongst faithful nodes, the harder it
      is for liars to impact their decisions.
    * But consider just two faithful generals, who are split between
      ABORT and COMMIT. A vote either way will swing their decision.
    * A liar can enter and vote COMMIT to one node, and ABORT to the
      other.
* How could you stop this?
    * You want to get consensus on what each node `i` is saying; the
      other `n-1` nodes need to run an algorithm to decide what the
      hell he said.
    * The hope is that, if a enough honest participants try to decide
      this problem, we'll get a consensus answer.
    * If the node is a liar, it doesn't matter what we think it says,
      as long as we think consistently.
    * If the node is faithful, then we really need to agree that it
      said what it meant.
        * We don't want this node's value to be *confounded* by liars.
* That's the same problem, just one smaller!
    * We always need consensus. And if all honest nodes have the same
      start value (i.e., they are all told the same thing by an honest
      node), that must be the choice.
* Think about for `n=3`.
    * Two nodes are trying to determine what a third said.
    * Two possibilities:
        * Two honest nodes are trying to decide what a liar said.
        * One honest node and one liar are trying to decide what the
          honest node said.
    * So a lie would be caught, but a traitor could confound the
      intent of an honest node!
* OTOH, with `n=4`, `f=1`:
    * For each node, we try to figure out what they said.
    * We'll just take the majority vote of what that node told the
      others.
    * Even though the liar can participate in the 2nd vote, they can't
      swing the election, which will always be 2-1.
* The bigger `n` is, the better!

**Starting to Induct**

* Let's think for `f=2`, `n<7`:
    * If `n=4`, the faithful generals aren't even in the majority. The
      traitors can subvert the vote without even lying.
    * If `n=5`, then when we check a faithful general's answer, `n=4`,
      `f=2`. Then, when checking an honest general's vote, the
      faithful generals are not in the majority.
* Consider `n=6`:
    * If we used majority rule, then an honest general's vote cannot
       be subverted (since honest checkers are sure to be in the
       majority). That's because `f=2`, but `n-1=5`.
    * The problem is checking the work of a liar!
    * We can't use majority, since a liar can tell two honest
      generals COMMIT, and another two ABORT. The last liar can
      push them both in different directions.
    * And importantly, we can't pick and choose. We need one strategy
      that works regardless whether we are checking the work of a liar
      (since we don't know!).
    * So we could try to use our `f=1`, `n=5` algorithm all the
      time. This would be able to catch someone who lied at the top
      level.
    * But an honest node's message could be confounded, because when
      we recursively try to solve `n=5`, `f=2`, even when our three
      honest nodes all agree, we're left with two liars who can
      confound our second round checks!
* Last, let's consider `n=7`
    * Each of the nodes will submit their vote to the others.
    * We'll check each vote using the old algorithm:
        * The six other nodes will try to get consensus on what was
          said.
        * They'll do this by telling each of the others what they
          heard.
        * We'll then check each of these messages, using majority
          voting.
    * If the original node was dishonest, the old algorithm applies.
        * There are six honest nodes, with different values, and one
          liar.
        * If the next node was dishonest, five honest nodes reconcile
          their lies to get an answer.
        * If the next node was honest, it's four honest nodes versus
          one. The liar can't change the vote now.
    * If the original node was honest, the old algorithm technically
      doens't apply.
        * If the next node is honest, then we have three honest nodes
          and two liars. But the three honest nodes all agree, and the
          liars can't change anything.
        * If the next node is a liar, then the three honest nodes can
          disagree. The last liar can trick some of them into thinking
          COMMIT, and others ABORT.
        * But that doesn't matter. Because the honest nodes will still
          be in the majority, because they are decided correctly.
* I would say that, if the margin of victory amongst honest nodes is
  greater than `f`, then normal voting reaches the majority decision.
    * When we recursively run the algorithm, checking a dishonest
      node, we may have a low margin of victory, but we got rid of a
      liar that can't continue to confuse us in future rounds.
    * When we run the algorithm recursively, checking an honest node,
      we have a very high margin of victory. We may not be able to
      detect the lies of the remaining liars, but if our margin is
      high enough, it doesn't matter.
* Last, we know that we need `n>3f`, else we could solve for `n=3`,
  `f=1` by simulation.

## Formal Algorithm

* EIGByz:
    * Construct the exponential information gathering tree.
    * Ignore messages that are just garbage; you can put in a default
      value for that. We're focused on lies.
    * The leaves have values, work your way up, taking the majority of
      the a leaf subtree. These are everyone's votes about what
      someone said.
    * Run this for `f+1` rounds.
* Consider when we do this at the fringe:
    * All messages from honest nodes recorded in the fringe should be
      the same everywhere. Fringe messages from dishonest nodes could
      be different in different nodes.
    * Let's induct one level up. When we assign a value to the honest
      node one level up, we look at its children.
    * How many of its children are honest? Hopeful a majority! The
      last node represents what a process heard via a chain of `f`
      processes. That means there are `n-f` votes here. Since `n>3f`,
      this number is greater than `2f`, so even `f` liars here cannot
      subvert this vote.
    * This is enough to establish that, given everyone wanted the same
      value to begin with, we'll end up with that value decided
      collectively at the end.
* But what about *split decisions*?! Will we reach agreement there, or
  will be be confounded by traitors?
