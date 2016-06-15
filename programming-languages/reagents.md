## Background

* Coarse grained locking is unscalable.
    * Serializes operations.
    * Extra cache-coherence traffic, since you have to acquire a lock
      exclusively even before you do an update.
    * Stalls of a thread holding the lock affect other threads'
      throughput.
* You can try to make the locks more fine-grained.
    * This allows more parallelism.
    * One potential issue: the finer the locking, the more locks must
      be acquired, which means more cache misses, since to acquire a
      lock requires to get it exclusive (and then modify it).
* Nonblocking
    * Hard to write nonblocking code.
    * Mention an improvements to Treiber stack. On a "sidechannel" a
      failed pusher can advertise an available element, and a popper
      can take this element if they fail to take from the main stack.
        * This is valid because it will be as if these operations
          happened in sequence.
        * After a while, the advertised sidechannel push will timeout
          and you can try to do a normal push.
        * This will tend to alleviate head contention, but only if
          there are also poppers.
        * Hopefully pushers will not greatly exceed poppers, else
          memory use will be unbounded. Though in a short-term this
          might be true...
    * He discusses the problem of "partial" operations. A `push` and
      `tryPop` are valid because they don't have to block for
      anything; they may have to retry because of temporary CAS
      interference, but they don't block indefinitely (well, you know
      what I mean...).
    * But if you want to block, nonblocking alone isn't going to give
      you this. You could busy-loop, but that doesn't seem desirable.

## Reagents

I think the idea is this: computations are represented as data. But
then you can combine this via sequencing. That is: a function dosn't
*do* anything, it just returns an operation.

Experts can write the fine-grained operations, and users can
combine/sequence them.

Your operations can be partial functions: they require a certain state
before they can complete. In that case, the reagent library will block
for you.

Okay, I got confused/bored. I should probably look at this more; it
does seem pretty fascinating.

Source:

https://www.mpi-sws.org/~turon/reagents.pdf
https://news.ycombinator.com/item?id=11907584
