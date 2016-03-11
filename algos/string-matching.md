## Naive Search

In naive search, to find a pattern of length `m` in a string of length
`n`, it takes `O((n-m)m)` time.

## Rabin-Karp

Rabin-Karp uses a hash function where we choose a base (typically a
large prime), and use the bytes as digits in this base (e.g., ASCII
values of string) in a little endian manner. To "roll" the hash, we
subtract the last digit, then multiply by the base, then add a new
"ones" digit. They also want you to divide this by polynomial by an
irreducible polynomial to basically map it into a smaller space. This
is called a *Rabin fingerprint*.

The math isn't that important. It's a rolling hash.

To produce these hashes at each index takes `O(n)` time. Then you hash
your pattern, and do a comparison to each of the hashes. This takes
`O(m+n)` time.

You then need to "double check" for every possible hit. Obviously you
could have a false hit at every index, but it is far more likely that
every hit is a true matching. So the time is linear in the number of
hits. But of course the worst case is that you get all spurious hits,
in which case performance is as bad as naive search.

A few more notes. Sometimes we say tht there is a "preparation" time
of `O(m)` while you cmpute the hashes for the pattern, followed by
`O(n+km)` time to check for hits in the string. The idea is that you
may search for the pattern in many strings, but you only pay the
preparation time once.

Both KMP and Boyer-Moore have better worst case bounds than
Rabin-Karp, and can have better average performance. However,
Rabin-Karp is good for matching *many* patterns. The idea is to hash
all the patterns, and put these in a hash map. Then, as you compute
the rolling hash, you just do an `O(1)` lookup for any hits.

This apparently is used often for plagiarism, since you can check for
many substrings simultaneously against a database.

## Knuth-Morris-Pratt

KMP notes that, when a string fails to match, you can often "skip
ahead" to a next possible index.

Let's think about this. Say the target is `ABCDEF`. Let's say that we
start checking the string at position `i`. We find `ABC` successively,
but `i+3` doesn't match `D`. I claim that we can immediately advance
to next consider `i+3` without trying to start at `i+1` or `i+2`. The
reason is that, if we matched positions `1` and `2` in the target, the
first letter cannot start at either of those positions.

Let's consider another example `ABCABD`. Let us say that we match up
until `D`, but that this doesn't match. We should skip ahead to start
trying to match the target at the second `A`: position `i+3`. But note
that we don't need to check position `i+4`; we know that's a `B`. So
we continue at `i+5`, to see if a `C` is there.

Let us set aside computation of this table for a moment. I argue that
using such a table, we can match a string in `O(n)` time. I argue that
each time we find a match, we move forward our test index, while each
time we fail a match, we move forward our start index. Since these
range through from `0...n`, we can only ever consider each index
twice.

**Computing the table**

We need to compute a jump table: basically, if we've matched up to idx
`i` of the pattern, but then encounter a mismatch at idx `i+1`, how
much do we advance the start index?

Note that this effectively asks: given the substring `s[0..i]`, what
is the length of its longest suffix that is also a prefix? Given that
this is `k`, we want to advance the start index by `(i+1)-k`.

Consider `ABCABD`. Say that we have matched up to index 4, but then
index 5 doesn't match. The length of the longest prefix/suffix for
`s[0..4]` is 2, so we advance the start index to 3.

Let's consider how to compute this in `O(m)` time. Let's use strong
induction: that we've calculated this for all previous indices, and
just need to extend our solution.

Consider that we know the length of the longest prefix/suffix ending
at `i-1` is `k`. Then let us check whether `s[k]==s[i]`. If so, we
know that we can extend the previous prefix/suffix by one more
character.

So of course let's assume not. We need to find the next shortest
prefix/suffix. How to do that? Well, consider that `s[0...k]` is the
prefix/suffix from before. Then the longest prefix/suffix *of this
substring* is the next longest prefix/suffix at `i-1`. We've
previously calculated this, so we look it up; call this `k'`. We then
ask whether `s[k']=s[i]`. We repeat, until this reaches zero.

I argue that this is `O(m)`. That's because, each step is either
extending the best prefix length by one, or reducing it. The trouble
seems to be that at any step we could reduce the prefix length many
times. But the idea is that, overall, we can only reduce this prefix
length a total of `m` times, because that's the longest it could ever
get.

## Boyer-Moore

This approach tries to do jumps. Note that even KMP doesn't try to do
jumps.

Take a pattern `aaaaaa`. If you line this up with the text, and don't
find an `a` at the last index, then you can immediately jump the start
index by six characters.

So as preprocessing, calculate a table indexed by `(i, c)`. This table
stores an index `i'` where `s[i']` is the last instance of the
character `c` to the left of the `i`. Basically, this table is telling
you how much to jump the start index on each mismatch.

This is sometimes called the "bad character rule" table. A second
table is also used for shifting; we will take the maximum of the
values returned by the two tables. This is the "good suffix table"; it
is indexed by a length `k` of the suffix matched so far. It stores the
how far to move the start index to align the second-to-last instance
of this suffix. If the suffix does not occur twice in the pattern,
then we can move up the start index at least to one past `m-k`, the
start of the suffix. In fact, we may be able to shift forward even
more, basically we want to ask for the longest suffix less than `k`
that is also a prefix of the word (remember that?).

Anyway, we can compute these tables each in `O(m)` time. Each time we
skip ahead with a mismatch, we start again from the end. (There's
apparently an optimization called the "Galil Rule" that allows us to
avoid recovering old ground).

## Comparison

Rabin-Karp is the most useful when there are many search strings. KMP
is typically superior because it is `O(n+m)`, no matter the number of
matches.

Boyer-Moore can be very efficient; it can jump by as many as `m`
characters for each check, resulting in `O(n/m)` time in the best
case. Assuming we don't use the Galil rule (which results in linear
time), the worst case is `O(nm)`.

My understanding is that the Boyer-Moore works well with large
alphabets, like English. Then bad character skips can be large, and
good substring skips also. Especially when the string we are searching
for is longer, this can mean we avoid consideration of many characters.

Boyer-Moore works poorly for small alphabets (like DNA), because bad
characters recur very soon, and short substrings commonly repeat.

That makes sense: it seems like BM would prefer large alphabets to
small; but won't KMP too? Why does BM do better on the former and KMP
on the later? Well, I don't feel very interested in this question...

**TODO**: Galil rule.
