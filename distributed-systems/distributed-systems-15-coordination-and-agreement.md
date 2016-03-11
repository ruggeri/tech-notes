## Network Preliminaries

You have a distributed system, and you want to make coordinated
decisions. You don't want master-slave, because that would mean that
if the master goes down, then the system stops operating.

A big difficulty is that the network is *asynchronous*. Asynchronous
means that just because you don't hear from someone doesn't imply that
they aren't operating somewhere. You can't use timeing to assume a
process has crashed.

It is typical to assume reliable communication channels, even if
processes are unreliable. TCP can make up for dropped messages, and
handle ordering. But of course the delay is not bounded, which is why
the network is async.

Sometimes network partitions can happen. For instance, a crashed
router can mean that two sides or the network lose the ability to
communicate. We'll assume that this will be eventually healed, and
that the two sides can continue communicating again. NB: sometimes
network partitions are asymmetric; one side can talk to another, but
not vice versa.

For now, we'll assume that processes fail only by crashing; we will
allow Byzantine failures.

## Failure Detection

Detecting failures is a problem. A failure detector is *unreliable*;
it can only say whether a failure of a process is suspected or
unsuspected; it can be wrong either way. A typical failure detector
method is to record pings. A more advanced method adaptively adjusts
the timeout window based on network conditions. Only in a synchronous
network can a failure detector truly be *reliable*. It will turn out
that unreliable failure detectors can still be useful.

## Distributed Mutex

The simplest way is to run a central lock server. If ever the server
or the client die, the lock will not be released.

Another approach is a token ring; a ring of processes, each forwarding
the token symbolizing that they are in the critical section. This
ensures fairness. However, there is substantial delay, since machines
are forwarded the token, don't need it, and then just pass it on.

Lamport's algorithm. When receiving a request, put it on a
queue. Immediately reply with an acknowledgment. When you have all the
acks, and are the first person on the queue, you have the mutex. A
process that wants to enter but doesn't have your ack can't enter. A
process that *does* have your ack can't have the request first on the
queue, since *your* request is first on *your* queue. They have to
have your request, in order to have acked it. When you're done with
the mutex, send a release message to have everyone remove this from
the queue.

Ricard-Agrawal have a simplification without remove. People who don't
want the lock immediately ack, people who have the lock delay acking
(put it in a local queue), and people who *do want* the lock delay
acking if the timestamp of their request is lower (puts it on a local
queue). The process knows its own request will be granted by the other
process. This can be superior because release requests are not
necessary.

We can help with the deadlock problem by having *timeouts*. If a YES
or NO response is not received promptly, we can consider that a
timeout. I don't know what the best recovery would be...

Why not just use process ID? For one, I assume fairness would be a
problem. For a second, I think this avoids the following problem: P2
requests from P1, P1 grants. P1 decides it wants the lock after all,
so it requests P2. P2 is still waiting for other acks; it grants P1
because it sees that P1 has the lower PID. But BAM because now two
people have mutually given each other permission.

A further refinement is Maekawa's voting algorithm. Here we, partition
the set of processes into subsets, all of which overlap. To enter the
critical section, you only need permission from one of these
subsets. Importantly: a process sits on a request if it has lent out a
mutex and doesn't yet have a release. Like Lamport, it needs to
maintain queues to reply in Lamport stamp order, otherwise deadlock
can occur. Presumably Maekawa has the property of eliminating a single
point of failure, while reducing messaging.

How does any of these approaches better handle failure than a central
server? Presumably by having crashed processes implicitly grant. Maybe
like a timeout. But that doesn't really work in an async network,
where messages can be dealyed, or partitions can happen...

## Election

This allows a single coordinator to be elected *without human
intervention*.

Ring-based election; circle around the ring, keeping track of the
lowest PID. If it comes back around and it's your PID, change the
message to "PID X elected". Cycle that until it comes back to
you. You're elected! This works if no failures can occur. The max
number of cylces around the ring is `3N`.

In a synchronous network, we can use the *bully algorithm*. We can
detect failures in the sync network when we don't receive prompt
ack. Anyone can start an election; they contact the higher PID
machines asking them to take over the election. If no one responds,
the machine elects itself and tells everyone it is taking over. Else,
everyonem more important tries to contact higher PIDs, if no one
responds, then they elect themself. Only the highest ID person doesn't
get a response, so he elects himself. This can result in a high number
of messages, but not necessarily a large number of rounds (since it
can be done in parallel).

## Multicast

For coordination, something like multicast seems useful. IP multicast
allows you to send a message to everyone in a group. You can make
delivery reliable by doing a TCP send to each process in the
group. This is of course terribly inefficient.

So we want *reliable* multicast:

* A message is deliver at most once.
* If a message is delivered to a process, it is eventually delivered
  to all the other processes in the group.
    * A weak form of agreement says that "if we send a message, it is
      delivered to all correct particpants"
    * A strong form is "if it is delivered to anyone, then it will be
      delivered to everyone". This holds even if it is incorrectly
      delivered by a process that crashes.

Here's an idiotic way to accomplish this:

* Sender does a TCP send of the message to each machine.
* On receipt, each machine does a TCP send of the message to every
  machine. This verifies that each machine receives the message.
* Only when this is done, does a machine know that *everyone* has
  received the message. We can now process the message in the
  application.

To make this more efficient, we can use regular IP multicast. What we
do is send not only the message, but the sequence numbers from each
sender in the group. This way, members of the group eventually learn
about missed messages. They can then send a NAK to either the original
sender of the missing message, or someone who has it. They can
retransmit. Messages can be queued (per sender) so that they are
delivered in order. (NB: ordered transmission isn't necesarily
required, in which case you don't need to queue).

The idea here is that this is kind of like TCP, but using IP
multicast, with the assumption that dropped messages are actually
pretty rare.

This approach isn't entirely practical: it means we have to keep
sending messages to become aware that we're missing one. Also, we need
to keep an indefinite buffer of past sent messages, since we never
know when we'll receive a NAK and have to retransmit. There are
approaches that solve this.

## Ordered (and Atomic) Broadcast

We saw that we can impose FIFO ordering (if a process sends m1 then
m2, they are delivered in this order) relatively easily for multicast
using packet sequence numbers. However, there are stronger forms of
ordering:

* Causal ordering: when a send of m1 "happens before" m2, even if sent
  by different processes, then m1 must be delivered before m2
  everywhere.
* Total ordering: all messages must be delivered in the same order
  everywhere.

Causal implies FIFO ordering. Total ordering does not necessarily
imply either. We can hybradize total and either FIFO or causal
ordering. Obviously the strongest is causal total ordering.

Ordering, even total ordering, does not imply reliability. However, we
typically call reliable totally ordered **atomic broadcast**.

Atomic broadcast can involve a fair bit of overhead, which is fitting
since it is a very strong form of synchronization.

To implement total ordering, we could use a *sequencer* process, which
will broadcast what messages should be received in what order; that
introduces a single point of failure.

One approach is to multicast a message, then have the sender collect
proposals from every receiving process as to the sequence number. The
sender picks the highest proposed sequence number and then sends
another message picking this. This ensures that the sequence numbers
will monotonically increase. This was proposed by Birman in 1987.

Birman gave a causal ordering protocol in 1991. We keep vector clocks,
counting the number of messages that "happened before" this message
from each process. This is effectively the last *sequence number* of
every received message at the sender when the sender transmitted its
own message. So we just hold the message in a queue until we've
received all those prior messages: that is, our sequence number is at
least that high for all the other processes. Note that this itself
doesn't insure total ordering: we don't totally order concurrent
messages.

Summarizing what we know how to do with multicast:

* Reliable unordered multicast (either by re-broadcasting, or NACK
  based).
* Reliable FIFO multicast (just adds sequence numbers).
* Reliable causal ordering (vector clocks).
* Total ordered multicast (sequence number proposals).

All these methods work in asynchronous networks. They do have some
requirements:

* Re-broadcasting needs to understand when a process is dead, and an
  ACK won't be received. Otherwise a message will never be delivered,
  because we can't know that it was really received by all live
  processes.
* NACK means that a sender really shouldn't die, else that message
  could be lost and won't be able to be retransmitted. Though other
  processes could keep this in their buffer, a message could be sent
  to one person but then the sender and the one receiver both die, so
  no one can replace the message...
* In the TO multicast method, if a process dies, they can stop *all*
  messages from being delivered, since the other processes need to
  hear back to know whether the sequence number was accepted. I
  suppose, to a limited extent, if no one hears back for a long time,
  they can (maybe?) agree to drop this and all subsequent messages
  from the process?

It turns out that reliable *totally, causally ordered multicast* is
*not possible* in an asynchronous network.

## Failures And Locking

The distributed locking algorithms we've seen are prone to
failure. Lamport and Ricard-Agrawal both require acks from everyone;
if any process is down, we don't get an ack, so we can't know they
don't have the lock.

We could treat a timedout request an ACK. But what if they have the
lock? We could have a lock with an expiration, but that's not safe,
because even if the holder checks that the lock is still valid before
their operation, it can expire in between the check and the operation
(e.g., due to GC pause).

Let's think of *why* you are using a mutex. Presumably there is an
underlying service, like a file-system, that you are writing to. If
the lock was issued with a token, the FS could verify that the token
is still valid when the client issues the write. It could reject old
tokens when a newer lock was issued.

This gets tricky when you want to write to *multiple* independent
machines; what if your token is still valid when writing to one, but
expires when you start writing to another? You can solve this problem
when working with one machine: all interaction with a token are in a
"TRANSACTION", which you commit at the end; if the token isn't valid
at the end of the transaction, then you rollback. But to do a
distributed transaction seems harder.

And the distributed case feels like the only interesting one, since
otherwise, why are you doing locking in the first place? If it's just
one service, just send one message to it. It can serialize that, in
which case you don't need a mutex. A mutex is only needed if you need
to coordinate two services.

## Consensus

So we've seen some distributed algorithms for:

* Mutual exclusion
* Leader election
* Causually (but not totally) ordered multicast

So let's start from a standpoint of allowing failures. Communication
is async, but reliable. We want to, in each round, decide the next
operation to apply to a distributed state machine. We want eventually
all operating machines to agree on the decided operation.

If there were no failures, we could have machines propose new
operations via reliable multicast. The machines would vote on the
first operation they received this round. We'd select the majority
winner.

What if machines can lie (as in BG). Then we can either required
signatures, which means the lies can be detected and discarded
(otherwise running the consistency algorithm). Without crypto, we can

Two kinds of failures: what if a machine crashes? How do we know to
stop waiting for their value? Worse, what if the machine lies, by
sending one value to some machines, but others to other machines.

We've seen a number of distributed algorithms; most of them are not
very resilient to faults. We want to explore algorithms which can
tolerate faults, but communication is reliable.
