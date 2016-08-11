Say want to represent items `0...Z`.

First approach is to keep a bitset. This uses space proportional to
the cardinality of the universe, though. However, this is an efficient
encoding if all subsets of the universe are equiprobable.

Because we can typically arrange things so that we store fewer fewer
items, we would like to be able reduce the space cost to encode
subsets with fewer elements. For instance, it may be desirable to have
a linear relationship between set size and representation size.

In that case, we can list codepoints for the set. This will use `n *
log Z` space. We can compress by keeping this list sorted, and then
using delta encoding with some variable-length encoding of digits.

Of course, the problem is that now all operations are slow. So we
prefer something like a hash set.

Let's do some back-of-the-envelope calculations. How many 64bit
numbers could we store in a hash set 1MB in size? The answer is
125K. That seems like a fairly large number.

Say we are Facebook. We have 1B users; each user has a unique
identifier. Say this is a base 16 number, each character in the
identifier is represented with 4 bits. Then we only need identifiers
of ~7.5 hex characers (~30 bits) to represent someone.

We need some room to add new users, so perhaps we might use say 32
bits. This actually will only allow 4B users, but that's probably fine
for our purposes.

Now we can start to consider the idea of friendships. A friendship
consists of two users, so we can represent it in 64 bits. Let's say
friendships are directed so we can't compress by eliminating
redundancy.

Let's say that the average user on FB has 20 friends. Then, the number
of friendships is 20 * 1B, and to store all these friendships requires
20B * 64bits. That's 160GB of friendship information.

If a machine needs to perform a job that tests whether arbitrary pairs
of people are friends, then it will need to do a disk seek per
friendship test. In fact, we prolly don't want to store this
friendship info at every machine, so we're talking about a *network*
request per pair.

TODO: if we use a bloom filter with efficiency 1.44 log(1/eps) bits
per item, we can acheive a 1% false positive rate with 23GB of
RAM. But that's still really bad...
