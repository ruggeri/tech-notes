FLP says that one faulty process can not be tolerated in an
asynchronous network. The faulty process can be fail-stop; doesn't
have to be Byzantine.

They mean *completely* asynchronous, BTW. As in: no synchronized
clocks or even timeouts.

What do they mean by "tolerated?" They want (1) correctness and (2)
*liveness*.

So this result is very similar to CAP theorem (Brewer's conjecture).

Paxos doesn't violate FLP because it isn't guaranteed to be live.
