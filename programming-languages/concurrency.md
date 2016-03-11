## Task-level parallelism/concurrency

Say you have a big vector and want to sum it. It is nice to have a
simple way to split the work amongst multiple threads to
accumulate. Go makes this very simple. You can take advantage of
multiple cores.

Likewise, let's say you have two independent IO bound operations to
perform to produce a result. You can easily do this concurrently, even
on a single thread.

## Scaling Independent Tasks With More Servers

Say you have to process many requests, each of which can be handled
independently and without shared state. A traditional solution is to
start many server processes and stick a load balancer in front. It's
okay for your servers to be single-threaded. You need many servers to
acheive 100% CPU utilization, since a lot of time is spent waiting.

In the past, this has had some problems because you use N times as
much memory per server. However, if you use COW and your program has
no state, you should be okay on this front. OTOH, garbage collectors
can play mean with COW.

But maybe load balancing isn't so easy. Not every request takes the
same amount of time. Round robin assignment can route requests to a
machine that's still busy. It's probably unreasonable to expect the
load-balancer to understand the actual load on a server and who will
next be able to service a request.

If the main cause of slowness is IO, then your server can be more
helpful if it doesn't block the entire worker on IO. Concurrency
primitives can be helpful here.

In JS you use callbacks to resume work when IO is done. This is
handled for you in go; a gothread can be paused as necessary.

**TODO3**: Why don't the workers just take work off a queue? This
should ensure good balancing, right?

## Long connections

The multi-process, single-threaded strategy supposes that requests are
supposed to be satisfied quickly. We saw that problems arise when some
requests take longer than expected: we need to add a load-balancer to
distribute work.

What if the connection is in fact long-lived: for instance, let's say
we are using websockets. The server may also collect state over the
course of the websocket.

At some point you can't run a socket per server. I guess the point is
that performance is bad even if you run a socket per thread? Enter
greenthreads/async ala Go/Node? I guess the scheduler gets angry if
you try to use too many threads? **Still find this surprising.**

## Locking primatives

Mutual exclusion is a property. We need hardware/software to acheive
it. A **lock** or **mutex** enforces mutual exclusion. Pthreads
provides one. It probably uses test-and-set/compare-and-swap.

Because pthreads provides mutex through the kernel, it doesn't
busy-wait and waste time slices. Instead, the kernel marks it as not
runnable. When the mutex is released, the kernel will mark it as
runnable.

A lock is a binary semaphore. A semaphore is a generalization of a
lock, where multiple resources are available.

I should note: there are algorithms that implement mutual exclusion
without special instructions. Lamport's Bakery algorithm is one
example.

**Condition Variables**: a common pattern is:

```
while (true) {
  take(mutex);
  if (predicate()) {
    break;
  } else {
    release(mutex);
    sleep(SOME_TIME);
  }
}
```

The idea is that the thread is waiting for some predicate to be true
before proceeding into the critical section. To do this, it grabs the
mutex and checks for the condition. If the condition does not hold,
then it needs to allow other people to work on the state, so it
releases the mutex, it then sleeps to give some time to the other
work, before trying again.

This is silly; effectively it's busy-waiting. To avoid this, you can
use a **condition variable** (an implementation is in pthreads). You
can register a lambda that determines whether you should wake the
thread. Now, instead of just releasing the mutex, a thread will also
check if it should wake somoene. It then signals the kernel to wake
this thread, and releases the mutex.

## Concurrency Approaches

* Processes
    * Lots of memory usage. Possible address space exhaustion.
    * COW should help, but maybe GC causes problems?
    * TLB flushes across processes? But does it need to do a lot of
      work saving stacks or whatnot?
* Native Threads
    * Callstacks take up space, but I don't see why it's so much.
    * Is the kernel so dumb that deciding what processes are runnable
      is so hard? Is it because of fairness?
    * Takes more effort to be thread safe. Have to trust all the
      libraries.
* Green Threads
    * Ideally built into the language is sleeping on blocking IO. Go
      and Ruby do this but not Clojure.
    * Less context switching?
* STM
    * Haskell, Clojure
    * Readers don't block writers.
    * Difficult to understand sometimes.
    * Not appropriate for transactions with side-effects.
        * Which happens all the time so WTF.
    * Clojure does this with *refs*.
    * Clojure also has *atoms* which don't use STM, they just do a
      compare-and-swap type deal. Atoms are more efficient, because
      you only need to lock/unlock one thing.
* Actors
    * Erlang, Scala
    * You send messages instead of functions.
    * Actors are *closed* (Rich Hickey's term) because they need to
      recognize the message.
    * Requires a second message to inform of internal state.
    * Works good for Raft.
    * Couldn't code be the payload of the message?
* Agents
    * Clojure
    * A function sent to an agent is performed asynchronously.
    * Can observe at any moment.
    * This is run in a thread-pool.
    * As ever, Clojure is stupid and won't realize if the function
      blocks, which stops any work on the thread-pool. Christ.
* Pure Async
    * Node, C
    * All I/O is async.
    * You have to pass callbacks *everywhere*. Infects everything.
* Promises/Task/Future
    * JS, Java, C#
    * Two typical choices: pass callbacks in, or return an object
      representing future result.
    * Promise standardizes interface, which is nice.
    * Provides a standard for error handling.
    * Sync fns can call async if they don't need the actual result,
      just the promise. Otherwise async infects everywhere.
* async/await
    * C#
    * Extends promises with syntax in the language.
    * Pauses your fn, then resumes it when the async call is done.
    * Allows you to write async code in a sync looking style.
    * But creates a 2nd class of functions you call in a weird way.
    * BTW: the `async` keyword is needed only because previously
      `await` was a valid keyword.
* Channels
    * Go.
    * Like a promise, but can resolve multiple times.
    * Like async in that waiting on a channel looks sync, but
      deschedules your thread.

So the notion of a thread is valuable: the async style, even when
band-aided by promises or async/await, is kind of a pain. The idea of
a thread doing some work sequentially in a normal way, but without
stopping someone else from doing their work, seems totally reasonable.

Mutable shared state was never okay with threads. Channels are not
some brilliant new idea; they're just a queue basically. But their
elevation to the center as basically the only way to communicate out
of a gothread means people are guided toward doing the Right Thing.

Scheduling is probably simpler/more efficient if gothreads can only be
blocked by (1) a read/write on an empty/full channel or (2) a system
call.

But why can't RoR fork a thread for every HTTP request? It would be a
light thread of course... I guess any code that wanted to block would
block all of Ruby? But isn't the answer to write wrappers to pollable
C code? Also, Go is going to have the same problem. Even Node can have
that problem if the C API writer is stupid.
