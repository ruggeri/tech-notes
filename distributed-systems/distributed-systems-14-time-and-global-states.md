## Timestamps

Time is important for figuring out a global ordering of events. But
time is difficult to establish because of clock drift. This makes it
hard to know, in a distributed system, what "happened before"
something else.

We can use Lamport timestamps to allow us to partially order events;
on each communication between machines, we send a program counter; the
other machine updates this if their counter is less. In this way, we
know that if one machine does something that happened before a
communication to another machine, it will have a lower Lamport
timestamp.

However, this method loses some information: things that didn't
logically happen before another event could have lower Lamport
timestamps. For this reason, we can introduce vector clocks, which
record a counter for each program. With this, we can say that
something happened before iff the entire vector is less than
another. Basically, we are keeping the most recent event of *every*
machine that logically happened before. In fact, this is the necessary
information to specify exactly when the event happened.

## Time

There are two failures of Lamport timestamps. The first is that they
aren't "universal". We need to always be passing these timestamps;
otherwise stuff outside the process is not controlled. For instance,
if computer A sends a packet to computer B, but I call computer B's
operator; my phone call can beat the packet! For that reason, we might
want a "universal" idea of time.

Also, time is itself useful; we want to be able to time out
operations; this needs more information than a Lamport timestamp. For
instance, what if we want to time out a mutex? We need to synchronize
clocks. Basically, since failure is only visible as "how long" a
message from a machine is delayed, time is clearly relevant.

Lamport presents an algorithm to do this, using pinging. The details
aren't hyperimportant (there are many other algorithms), but the idea
is that we can get synch errors pretty low.

The advantage now is that we can talk about "how long" something is
taking. This is useful in terms of timeouts: how long should it be
until we view something as not merely slow, but actually *wrong*. Like
a machine could get a mutex that times out.

## Global States

All of these require global knowledge:

* Garbage detection (liveness of objects)
* Deadlock
* Termination

The problem is primarily about trying to "stop" the world at a given
moment. The primary problem is that there are messages in flight. The
way to do this is to:

* Send a message to everyone asking them to record their state and
  forward it to the observer.
* On receipt the process will send a marked message to everyone,
  notifying that this is the last message it will send before the
  snapshot. Each process will mark all subsequent messages it
  sends. These happened after the observation request.
* The process will forward all unmarked messages to the
  observer. These were in-flight at the time of the observation request.

When are we done? If we assume FIFO messaging (with no loss), then
when a process receives the next marked message from a process, then
it knows there are no more "in the ether". This can be satisfied by
TCP.

Note that the entire state doesn't need to be sent to the observer;
instead, we can now run a distributed algo if we want, to verify the
condition. The important thing is we *pinned down* where we were at.

Note this algo doesn't interfere with the online process.
