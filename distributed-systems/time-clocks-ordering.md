Events in a single processor are ordered. Send of a message happens
before receipt of a message. This is a partial relation. Two unrelated
items are "concurrent".

Lamport then introduces the idea of a "logical clock." Basically, we
want `C(i)<C(j)` if `i` happens before `j`. To ensure this, we
increment our local clock whenever something happens. When we send a
message, we send our clock number. On receipt, if this number is
bigger than the local number, the local processor advances its clock.

In this way, there is an incrementing "global" clock. This defines a
total order; we might break ties by an arbitrary ordering of the
processors, if we wanted.

## Mutual Exclusion

You can use these Lamport timestamps to implement mutual exclusion.

Say you want to implement a multi-processor mutex. Assume that you
have ordered, guaranteed communication (like TCP). You can get that
just with packet numbering and acknowledgements.

Every processor has a queue. The queue starts with the message
`T_0:P_0 requests resource`.

* To request a resource, a processor sends `T_m:P_i requests resource`
  to all the other processors. The sender also puts this message on
  *his* request queue.
* On receipt of a request, a receiver puts the message on his request
  queue, and sends a timestamped acknowledgment.
* When a processor receives a release resource message, it deletes any
  messages requesting that resource in its queue.
* `P_i` knows it has the lock when a `T_m:P_i` message is first in the
  queue, and also `P_i` has received an acknowledgment timestamped
  from every processor later than `T_m`.

So okay. Say P2 timestamps his request after P1. This can only happen
if P1 sends P2 his transaction request *before* he receives P2's
transaction request. Thus, by the time P1 acknowledges P2's request,
P2 is already aware that P1 must complete his request first.

The basic idea: if any set of processors concurrently want the mutex,
then one of them had to send all the request messages *before*
acknowledging any requests; anyone else who acknowledged *any*
requests before their transaction has a higher request ID, and cannot
be the first to execute the transaction.

Note how important it is that no messages be received until all
request messages have been sent out.

(NB: if a process crashes with the mutex, then we can't know to unlock
it).

## Distributed State

Say that when a mutex is requested, an action is associated with this
request. Then, when the release command is sent, we know that there
are no other pending commands. On receipt of this release command, we
can execute the command. Note that release commands can come in out of
order, so we should pend release commands, and only execute
transactions in the queue order.

The only point of this 2PL strategy is to ensure that the command has
actually gotten out to everyone, and that everyone will not execute it
before every node has had a chance to propagate a lower ID
transaction.

## Failure

Lamport says that failure of a machine is indistinguishable from one
that is merely taking a long time. Therefore, it's about wall clock
time, not logical ordering of events. Therefore we're not going to be
able to quite attack that directly.

## Unification

Let's say I begin to try to take a lock. Then I call my friend, and
tell him to take the lock. It is possible that he will have a lower
transaction idea than my request, if my phone call beats my packet. In
fact, the problem isn't about the packet, it's about the fact that if
our clocks our too different, stuff outside the processor can happen
at the same "time" but not work right.

Let's say that all clocks run at a rate within `k` of each other; one
clock can run at the rate `1-k/2`, and another `1+k/2`, and that's
fine. In practice, most crystal clocks do run within a pretty tight
bound.

We want to devise an algorithm to keep the logical clock value
`C_i(t)` (at timem `t` on `P_i`), within `\eps` of `C_j(t)`. But if
two clocks run at different rates for a long enough period of time,
this is going to require a correction algorithm.

Let `\mu` be the minimum latency of communication between two
processes. We then want to ensure that `C_i(t+\mu) - C_j(t) > 0` for
any `i,j,t`; that basically says that by the time we get a message
from `j`, when `i` looks at the clock he will see a later time than
`j`.

We know that the message takes `\mu` time to propagate, we also know
that the sender's clock may be up to `\eps` ahead of the receiver's
clock. So typically we can only assume this will work if
`\mu>\eps`. But we need more, because what if `k>0` and causes the
receiver's clock to run slow? In that case, we'll *also* need
`\mu>\eps/(1-k)`.

Okay! We got our clocks working without anomoly *if* we can maintain a
small enough `\eps`: `\eps<\mu*(1-k)`.

## Time Sync Algorithm

Here's the idea. Whenever a message is received by a `P_j` at time
`t'`, then set `C_j(t)=MAX(\lim \delta\to 0+ C_j(t-\delta),
T_m+\mu_m)`. Here `T_m` is the time in the message, and `\mu_m` is the
known minimum delay. Then Lamport proves that if you send messages
often enough over links, then you can keep the time from getting out
of hand. There's a mathy proof of this, but I don't think I'm super
interested at this time. That's really like a time synchronization
paper, now.

## Reference

* Time, clocks, and the ordering of events in a distributed system
