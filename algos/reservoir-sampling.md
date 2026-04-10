Goal: Produce a uniform sample of `k` items from a stream of `n>=k`
items. The number of items `n` is not known in advance, which makes this
a _streaming_ algorithm.

Solution:

Take the first `k` items and put them in the array in order.

Then, for the `i`-th item, sample a random number `j` in the range
`0...i`. If `j<k`, then place this item at position `j` in the array.
Else do not take this item.

Proof:

Let's prove this inductively. Clearly the algorithm works for `n=k`
items.

Say this works for `i > k` items: we run the algorithm, and every item
has a `k/i` probability of being in the sample.

Now run one more time. The next item has a `k/(i+1)` chance of being
included. That's exactly the right selection probability if we end on
the `i+1`-th round.

But if the `i+1`-th item is selected, that means someone must be
ejected. It makes perfectly sense that we must select someone to eject
uniformly. That's exactly what happens when we pick position `j` to
eject from the reservoir.

Maybe it is slightly counterintuitive in the sense that some items in
the reservoir have "survived" more rounds. But entry is easier in
earlier rounds.

Math:

```
Selection probability for element already in reservoir
= (k/i)*(Prob no replacement + Prob replacement * Prob not selected for replacement)
= (k/i)*((i+1-k)/(i+1) + (k/(i+1))*((k-1)/k))
= (k/i)*((i+1-k)/(i+1) + (k-1)/(i+1))
= (k/i)*(i/(i+1))
= k/(i+1)
```
