## Stack

Persistent lists are easy.

## Queue

You can use the two-stacks method. But this means either (a) head or
(b) shift is worst-case O(n) (probably you prefer shift to be
O(n)). You can keep head O(1) if you aggressively (a) when pushing,
push to front when front is empty or (b) when shifting flip rear
whenever front becomes empty.

The O(n) shift ammortizes out, but it's still annoying. **I'm not
aware of a way to avoid this**.

## Doubly-Ended Queue

Easy extension of queue. Instead of flipping the entire thing, you
just flip half of its contents. Kind of annoying, because you have to
reverse the first half twice...

Everything ammortizes out again. But you have poor worst-case
performance again...

## Note on ammortization

Say we push `n` times into a queue, and then try to call tail n
times. Every call to tail is O(n). This is not desirable. And, in
general, it is painful that shift is amortized O(1), not worst case
O(1)...

The flip of the rear all happening at once is the bad part. Let's
pre-emptively begin the flip of the rear whenever the number of
elements ready in the front becomes equal to the number of elements
unflipped in the rear.

However, let's not flip the rear all at once. We'll move the rear into
a tmp buffer and begin lazily flipping it. This takes O(n) work. But
let's spread it over n shifts or pushes, spending constant time at
each step. At the same time, let's also be reversing the front, which
also can be spread over n steps, spending constant time each step.

After n steps, we have reversed the two fully. We can start popping
from the reversed front and pushing onto the reversed rear to
construct a new front.

If we were doing pushes into the queue the whole time, note that we
will be done in 2n steps, which is exactly when the rear reaches 2n
items, which means we are just ready to begin the process again.

If we did any shifts, that's okay. That means we have one less item to
shuffle flip from the reversed front onto the reversed rear. To
accomodate this, we should have copied the front stack in the
beginning, and kept track of how many items are shifted, so we can
stop that much earlier.

Note that if there are k shifts, then we run for 2n - k steps before
finishing reversing. The new front consists of 2n - k elements. The
rear also consists of 2n - k elements.

ALL IS WELL.

## Map

Use a BST. As you bubble up, do re-balancing. Only need to rewrite
nodes on the path, or those that get rotated around. Logarithmic time
complexity holds.

In some sense, this is as good as a hash map, because a hash function
needs to pop `O(log n)` bits.

## Priority Queue

You can use the same tree modification technique. However: you lose
expected O(1) insert (since you need to rewrite all the way to root).

Having seen the crazy trick with queues, you can probably avoid this
to. But I'm fucked if I care to find out.

## Persistent Vector

Clojure does this with what is effectively a BST, except all values
are stored at the bottom of the tree. The leaves store the vector
values in order. When you update at an idx, you just rewrite the
appropriate *path*. Get is similarly easy, and append just involves
maybe adding more leaves. Blah blah blah.

Of course, the depth is logarithmic, but Clojure deals with this by
using a base of 32.

I believe there are a couple tweaks, but this is the general idea.

I think the tweaks are:

* Store the tail stuff at the root so you don't need to chase down the
  tree and rewrite the path.
* Presumably this can be done with the head as well.
* There's also this thing called a "transient", which is mutable, but
  then gets "frozen" into a persistent structure.
    * Can do this by actually mutating the tail.
    * Okay, that's not actually what transients are, not even close.
    * But the general idea is that you know it's safe to mutate for a
      little while.

Note: updates on random elements appears to be approximately 10x
slower than `ArrayList`. So this isn't crazy fast. But neither is it
insanely slow. (Access at ends is much better, BTW)

http://hypirion.com/musings/persistent-vector-performance-summarised

## Hash Trees

Called a "tree" but really a trie.

Basically, a trie where the "strings" are the hashes of the keys, and
of course you can have some value associated. This is typically
implemented using a AMT, which is a micro-optimized specification of
the idea of a trie. I believe this is what PersistentHashMap uses.
