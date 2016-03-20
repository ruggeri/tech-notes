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
    * Their goal is less grand: with just a few liars, disrupt a split
      decision.
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
    * Notice right here: this didn't reduce to `n=3`, `f=1` when we
      checked the work of an an honest node! That's because in the
      second row, all honest nodes are in unanimous agreement. It
      makes it harder for a liar to confound this result!
* The bigger `n` is, the better!

**Starting to Induct: Intuition**

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
    * But an honest node N1's message could be confounded, because
      when we recursively try to solve `n=5`, `f=2`, even when our
      three honest nodes all agree, we're left with two liars who can
      confound our second round checks!
        * They can do this because when we want to check what an
          honest node N2 says N1 said, there are 2 honest nodes, and 2
          liars, so the honest people don't have a majority.
* Last, let's consider `n=7`
    * Each of the nodes will submit their vote to the others.
    * We'll check the vote from each node (I'll call it N1) using the
      old algorithm:
        * The six other nodes will try to get consensus on what was
          said by N1.
        * They'll do this by telling each of the others what they
          heard from N1.
        * We'll then check each of these messages, using majority
          voting.
    * If N1 was dishonest, the old algorithm applies.
        * N2,...N7 all vote what they hard N1 say (what they hard from
          N1). They want to come to consensus on what N1 said.
        * This is five honest nodes, and one liar. So the `f=1`
          algorithm applies.
    * If N1 was honest, the old algorithm technically *doesn't* apply.
        * Because N2,...,N7 contains four honest nodes with two liars.
        * But because all honest nodes should be unanimous, I argue we
          can still run the algorithm.
        * Say N2 is honest; it sends N3,...,N7 what it heard from N1.
            * N3,...,N7 are going to take a majority vote on what they
              heard.
            * N3,N4,N5 (the honest nodes) will all be voting the same.
            * N6 and N7 (the liars) cannot swing the election, since
              the margin of victory is so high.
        * I'm going to stop here. I argue that it doesn't matter what
          we think a liar node says N1 says, since all honest nodes
          will be in agreement! The marging of victory is so high!
        * Say N2 is dishonest; it sends N3,...,N7 lies about what N1
          says.
            * In particular, N3,N4,N5,N6 (honest nodes) could have
              heard *different* things from N2. N3,N4 may have been
              told by N2 that N1 said ABORT, while N5,N6 heard COMMIT.
            * So we're split two-two, which means we're ripe for N7 to
              come in and lie, telling two nodes that it heard COMMIT
              and the others ABORT.
            * As I said though, it doesn't matter, because the margin
              of victory is too high!
* I guess a lemma would be that you can run the consensus algorithm
  even with `f<n/2` failures *if* all functional nodes are in
  consensus.
* BTW, we know that we need `n>3f`, else we could solve for `n=3`,
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
* Some numbers:
    * After the first round, there are `n` paths (what you heard from
      each node), then `n-1` paths (what everyone thought they heard
      from N1), etc.
    * That means, at the fringe (layer `f+1`), there are `n-f` nodes
      voting on what they heard.
    * Think about this for `n=4`,`f=1`; it makes sense, there are 3
      nodes voting on what the fourth said.
    * Note that, at the fringe, honest nodes are sure to have a
      majority over traitors: `n-f>2f`.
* Lemma: All honest fringe nodes are the same:
    * Say we have an information path ending with an honest node
      (i.e., "an honest node Ni told me that ...").
    * Ni tells everyone the same thing. So everyone gets the same
      data.
* Lemma: Everyone computes the same, correct value for an honest
  interior node:
    * We'll do induction. Take a node right above the fringe. Call it
      the path `n\bar = n_{i1},...,n_{ik}`.
    * We know that `n_{ik}` honestly communicates its value to
      everyone.
    * So every process has the same value stored at `n\bar`.
    * Let's extend this path by one, to get to the fringe. This
      represents what we heard from `n_{ik'}` about what they heard
      from `n_{ik}`.
    * As before, all truthful `n_{ik'}` will tell everyone the same
      thing, so we'll all have the same value here.
    * And this will be the *correct* value, since we have a majority
      of honest nodes here!
    * We can induct. Given a path `n\bar`, we know it will communicate
      its value down to all child nodes. We know that the children
      will all resolve to this value. And we know there are enough
      honest children to be an outright majority.
* Okay, we've shown we'll reach consensus on what an honest node says,
  but what about a *dishonest* node?
    * We know that they can't subvert what honest nodes say, from the
      above lemma.
    * But here's the rub. A dishonest node tells half the honest
      people one thing, while telling the other half another.
    * And the kicker is that a second dishonest node lies to push
      these people in different directions!
* But we'll prove that can't happen. Consider any path of `f` liars.
    * At the end, there are only honest nodes, so we'll reach
      consensus on a majority value for the last liar.
    * But then that bubbles up. We'll reach consensus on the next
      liar, etc.
* This finally proves that, at the top level, we get the right value
  for every honest node, and a consensus value for every liar.
* That means the liars can't stop consensus on a final value, and
  we'll do the right thing in with unanimous honest voters.
* Why `f+1` rounds?
    * After `f+1` rounds, we know either:
        1. There was an honest node somewhere on the path, OR
        2. Only honest nodes are left at the end.
    * We couldn't have less than `f+1` rounds, because then we could
      be deceived by a liar (tells half the people one thing, another
      half another, and a liar to swing).
    * We couldn't have *more* than `f+1` rounds, because then, for a
      string of truth tellers, the liars would be in the majority and
      could make us think the last truth teller was lying.

**TurpinCoan**

* An optimization for when we're deciding a non-binary value. It will
  add a couple of rounds, and then run the standard Byzantine
  algorithm, but with a binary decision.
    * Can reduce the message requirements if the size of the value
      agreed is large.
* Round 1: Broadcast your vote. If you receive more than `>=n-f` votes
  for a value, choose that as your candidate, else choose null as your
  candidate.
    * Lemma: Only one such candidate can be chosen, at most.
    * For some people to think that `candidate=c_0` while others think
      `candidate=c_1`, we must have `>=(n-f)-f` *honest* votes for `c_0`
      and `c_1`, while the liars come in and lie with their `f` votes
      for `c_0` with some and `c_1` with others.
    * But there are only `n-f` honest votes to give overall. And `f`
      is less than half of `n-f` (by assumption `n>3f`). So some
      honest voter would have to vote twice, which is not possible.
    * So some honest nodes might think `candidate=c_0` and others
      `candidate=null`, worst case.
* Round 2: Broadcast your candidate. If a candidate receive more than
  `>=n-f`, set `vote=1`, else set `vote=0`. Also, we set candidate to
  the candidate with the most votes (maybe just a plurality).
    * Important note. Votes for `null` are disregarded. So `null` can
      never win the election, even if it gets the majority of the
      votes.
    * Lemma: It is clear that if all honest nodes agree on `v`, this
      will be the final candidate. They will vote for consensus and
      get it, establishing the *validity* property.
* We now Byzantine consensus to see if we might have a unanimous
  decision. If we do, then we'll use the majority winner, else we'll
  use a default value.
* To prove correctness, we need to show that we always reach a
  unanimous result. If the Byzantine consensus rounds fail, we'll pick
  a default, so we really just need to show that: if Byzantine
  consensus succeeds, everyone picked the same candidate.
    * If consensus succeeds, someone got `>=n-f` votes for `v`.
    * Since `>=n-2f` of those votes are honest, any other honest
      processor got `>=n-2f` votes for this.
    * We know that all other honest votes were for null and are
      disregarded. So we just need to show that the `>=n-2f` honest
      votes we get for `v` will be greater than the `f` lying votes we
      get for another challenger.
    * But we have that because `n>3f`.
    * So even though an honest processor might not also vote to choose
      this value, he at least understands what value we're talking
      about.
* I would say the idea here is that you want to say that you'll only
  vote to commit if there is such a supermajority that you know that
  there is no way this choice could not be the majority value.
    * Normally you can't know that, because even if `n-f` nodes are
      voting for your value, when you subtract out liars, you don't
      know if other people are seeing `2f` votes versus `n-2f` votes.
    * So the way you do this is you don't allow the liars to misuse
      the honest minority.

**ConsistentBroadcast**

* Let's think of a broadcast algorithm resilient to Byzantine failure
  (not necessarily lost messages).
* Typically you'd just have the sender send the message to everyone,
  and they deliver the message.
    * But then a traitor could send some people the message, but not
      others.
* Therefore, to show that everyone has actually received the message,
  we could say that on receipt of the initial message, we send an ACK
  out to everyone. We wait until we have `n` ACKs before we deliver
  the message.
    * This is a typical strategy for broadcast when losing messages.
    * The problem is that a traitor could ACK to some people but
      withhold it for others others!
* For that reason, we lower the requirement to `n-f` ACKs.
    * Now when an honest process broadcasts, we know that its message
      will get through to everyone.
* But this opens up a potential hole for a dishonest process to again
  send a message to some processes but not others.
    * It could send the message to `n-2f` honest processes. This means
      that `n-2f` ACKs will be sent out to everyone.
    * Then the `f` traitors can ACK a subset of the honest processes,
      pushing them over the limit, but not others.
* A way to fix this would be to have honest processes ACK not only on
  initial send, but also send an ACK in response to an ACK, if they
  haven't sent one already.
    * Then, if a process accepts a message, it will have forwarded
      this on to everyone else, and they will all ACK too, so they
      will also collect `n-f` ACKs eventually.
* I think the problem here is that if you send a response ACK as soon
  as you get a ACK, this allows forgery.
    * Traitors can send an ACK out of a message a process didn't send.
    * Then an honest process ACKs it, and everyone eventually accepts
      this.
    * If instead, you set a floor of `f+1` ACKs before you ACK a
      process, then at least one honest process needs to ACK
      first. This means this honest process must have received the
      message in an initial broadcast.
* If a message is accepted by an honest process, it will be accepted
  by everyone promptly (within one more round).
    * To accept a message, we have received ACKs from `n-f` processes,
      of which at least `n-2f` are honest. This is `>=f+1`, so
      everyone in the network must have received `>=f+1` ACKs. Thus
      they will all in turn ACK, meaning everyone will have `>=n-f`
      ACKs by the end of next round.
* NB: there is a weakness to this scheme. Even though everyone accepts
  a message within one round of each other, this may occur at some
  arbitrary time after the start of the broadcast (only when a traitor
  initiates the broadcast).
    * For that reason, if you try to set an arbitrary upper bound on
      when a broadcast should "timeout", you'll introduce a situation
      where some honest nodes could deliver the message, but not
      others.

**PolyByz**

* This will be built on top of ConsistentBroadcast.
* A simple approach would be for everyone to broadcast their vote. The
  majority winner would be selected by all processes.
* You might think this single broadcast would take a couple rounds:
  first you broadcast, then they echo, then the messages are
  delivered, and then we can count votes.
* However, that won't work. A malicious process can cause a vote to be
  delivered in the second round, in which case we have to wait for a
  third round so that the message can be delivered to others. But then
  a malicious process could cause a message to be delivered in *that*
  round, requiring that we wait for a fourth round...
