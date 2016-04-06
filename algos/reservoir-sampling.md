Produce a sample of `k` items from a stream of `n` items.

For the `i`-th item, sample a random number `j` in the range
`0...i`. If `j<k`, then place this item at position `k` in the array.

Let's prove this inductively. Clearly the algorithm works for `k=n`
items.

Say this works for `i`: we run the algorithm, and every item has a
`k/i` probability of being in the sample.

Now run one more time. The next item has a `k/(i+1)` chance of being
included. OTOH, that means someone could be ejected. The previous
elements have a:

```
(k/i)*(Prob no replacement + Prob replacement * Prob not selected for replacement)
(k/i)*((i+1-k)/(i+1) + (k/(i+1))*((k-1)/k))
(k/i)*((i+1-k)/(i+1) + (k-1)/(i+1))
(k/i)*(i/(i+1))
k/(i+1)
```

Basically:

* You have to sample the last element at `k/(i+1)` probability (to be
  uniform).
* Selection for who to reject needs to be selected at `k-1/k`
  probability (to be uniform).
* Then this *has* to be correct:
    * You've selected the last element with the right probability.
    * All the other items are selected equiprobably.
    * So they must all have the right probability too!
