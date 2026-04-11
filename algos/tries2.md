# Tries

Sometimes called a **prefix trie** or **prefix tree**.

If storing strings, each edge out corresponds to a next letter. If
bitwise trie, it's a one or a zero. A trie can store a set or a map.

**Implementation: Successor Nodes Array**

A naive implementation allocates an array of node pointers with length
equal to the size of the symbol space. That allows the quickest lookup
of the successor; you index into the array by the successor token, and
then go to that node (or identify it as NULL).

Note that you need a boolean like `is_end` because a key can both be
present, AND be a prefix for a longer present key.

Ideally, you want depth of the tree to be short. That suggests a large
symbol space is an advantage. For multibyte values, you might use a byte
as a symbol (branching factor of 256). This gives a very shallow tree,
requiring less jumping through the tree.

At top of tree, maybe most of the successor node array is filled. But
especially in the middle (and often at the end), the array is mostly
empty, which is a lot of wasted space. For that reason, practical
implementations might use smaller, sub-byte symbols. An extreme is
_bitwise_ tries, where a character is one bit.

You often want to trade-off these factors: excessive jumping, and
excessive space wasted on internal nodes. Clojure uses 5-bit chunks as
symbols. Keys are typically 32bit hashes, which means last node storing
two bits of key actually only stores at most four values.

**Successor Node Array: Alternatives**

You could run a linked-list of `(char, pointer)`. But this is wasteful
in terms of pointer overhead, and it might involve a lot of chasing
through pointers simply to find the successor node.

Later we will discuss many other alternatives further, because this is a
major design decision.

**Use To Implement Other Data Structures**

Tries are often used to store variable-length strings of characters, but
they can also be used for binary quantities, with each "character" being
a fixed-length chunk of bits. When the "key" stored is a hash value, a
value can be associated, in which case the trie is acting as a hash map
implementation. When used this way, we call it a **hash trie** or even
**hash tree**.

Clojure relies on hash tries to provide `PersistentHashTree`. Even the
Clojure vector implementation `PersistentVector` uses what is
effectively a trie where keys are an index in the vector.

**Versus Hash Table, vs BST**

In a typical implementation, to search a string of `k` characters, it
takes `O(k)` time to do a lookup in a trie. That's the same as for a
hash map (takes `O(k)` time to calculate `k` bits of hash). But the trie
will often have more overhead, and typically involve more pointer
chasing.

Trie is a persistent datastructure, which can be be an advantage in some
contexts.

A trie might be very good for iterating all entries with a given prefix.
But a BST can do that, too...

Trie never needs rebalancing like a tree does. Trie never needs to pause
to resize the bucket array and rehash all items like a hash map does.

## Array Mapped Trie (AMT), Hash Array Mapped Trie (HAMT)

The default, naive implementation of a trie uses an array of successor
nodes. So AMT and HAMT are slightly silly as names.

Here is the idea of an **array mapped trie**. Each node consists of a
bitmap with one bit per 32 children. It's one if a child is present.
There is also an array with an out-edge per _present_ child. This means
no nulls must be stored.

To check if there is a successor for a symbol, you use `popcount` and
look at that bit in the bitmap. If zero, there is no successor.

To find the successor, you then mask `popcount(key) - 1` with the
bitmap. You then do a `popcount` of _that_. This gives you the count of
present nodes for smaller keys. You use this as an index into the array
of present nodes, which is kept sorted by symbol.

This saves space for nodes, and involves just a few extra CPU
accelerated bit operations.

The downside is that to add a successor node, you must rebuild the
successors array. So this implementation favors a read heavy workload
(which is typical). As an optimization, Clojure will switch to the naive
implementation (with NULL values stored in the successor node array)once
you hit 16 present successors.

We call this a **hash array mapped trie** (HAMT) when we are using an
AMT to store hash values. Basically: a hash tree using the AMT storage
idea.

I believe this was developed by Phil Bagwell. It is used a lot in
Clojure, and I believe Scala, too. I once read his paper; I previously
noted that I _did not like_ that paper.

An alternative would have been to have an array of out edges, but also
storing the 5bit edge-value. But that would have involved more overhead,
and iteration to find the appropriate out edge to take.

A _ctrie_ is a lock-free ("concurrent") version of a HAMT. It allows
concurrent modification without locking. It is beyond our current
discussion scope, though.

**PersistentArrayMap**

- This is more Clojure specific, but when you expect a small number of
  keys (<16), and you want low overhead, a `PersistentArrayMap` is an
  array of up to 16 cells to hold up to 16 items.
- When operating in typical immutable mode, you copy and recreate vector
  as you insert keys.
- When you exceed the threshold, it will automatically promote itself to
  a `PersistentHashMap`, which is basically a HAMT.
- The `hash-map` function in Clojure chooses `PersistentHashMap` by
  default. But literal values are usually constructed as a
  `PersistentArrayMap`. This makes sense because most literals are not
  further modified.
- Clojure uses a trick/optimization. The array of nodes actually stores
  `(keyOrNull, valueOrNode)` pairs. This is basically a "union" kind of
  concept. The idea is that, if there is only _one_ successor for a
  bitstring, it will be stored directly here.
  - This can be a big advantage for HAMTs, since tree becomes very
    sparse as you descend.
  - Eventually you are jumping nodes, even though there is only one key
    to descend to. That wastes both memory and cache.
  - Only on insertion of a second key with tops bits matching, you'll
    create a new node to hold both values.

## Ternary Search Tree

Basic idea is going to be to (1) avoid excessive memory use by naive
array backed implementation, (2) avoid slow iteration of children by
replacing array implementation with LL.

TST implementation will be:

- A `char` for the present token.
- `left` for a node that replaces present token with a lesser token.
- `eq` for a node that _extends_ present value.
- `right` for a node that replaces present token wiht a greater token.

This is "ternary" because there are three "children", though left/right
are not really children, but siblings.

Insertion/search is relatively naive. Descend like a trie. To descend to
next node on path, you may have to move left/right from the stored `eq`.
I believe the idea is that if `eq` tends to be approximately the median
value of the children, then you'll have approximately `O(k log C)`
lookup, where `k` is the length of the string, and `C` is the size of
the alphabet. So this is "better" than a LL representation of children
only in the sense that you get to jump into the middle...

I suppose a TST can be better than a BST if there is a lot of space
savings via structure sharing of common prefixes. OTOH, you have a lot
of pointer chasing sideways if branching factor is relatively high and
there are a lot of extensions of current prefix. Then again, a BST has
low branching factor so higher depth, which also means a lot of chasing
downward...

Doesn't feel like an amazing advantage over BST, which is also fairly
good at prefix matches and iteration of completions. You do get to avoid
repeated initial prefix comparisons (as with BST), because a subtree of
TST _always_ represents completions of a prefix. And you maybe get
savings from avoided repeated re-representation of initial prefixes.
Probably storage efficiency of TST grows with density of strings. But
honestly, TST may be a niche application...

## Radix Tree, Compressed Trie, PATRICIA Trie

**Radix tries** (also called **compressed tries**) allow a parent-child
edge to have multiple characters. In a naive implementation, a node adds
a _string_ field like `key`. Children can still be indexed by next char.
The choice of array node storage, linked list, or the AMT concept is all
relatively orthogonal to the radix trie idea.

When there are "gaps" in the trie - where there is only a single
extension - you can save levels of nodes. This saves the memory to store
those intermediate nodes, but every node will have to be a bit larger.
You save some pointer chasing about memory, though there might be
indirection for storage of the string. You can probably be careful with
unions to avoid the worst of this.

When searching/inserting, you need a little extra logic to check whether
the next node actually matches not just the next character, but maybe
even more characters.

In this setting we call the size of the alphabet the _radix_. A _binary_
radix trie has at most two children, of course; it is basically just a
bitwise trie, with this multiple-character technique.

A binary radix trie is called a PATRICIA trie (it's an initialization).

You could see this idea being useful for internal autocomplete:
completing not the rest of the word, but maybe just the next part of the
word (one prefix extension, multiple suffixes).

## Suffix Tree

**TODO**: Finish reviewing me!

Suffix tree for a string `S` is a compressed trie all
suffixes. Suffixes end in `$` so all values stored at leaf nodes.

Allows rapidly finding whether a substring. That's because any
substring is the _prefix_ of a suffix. Which means that we can just
travel down the tree with our query.

Likewise easy to see if a string is a suffix, of course.

Can count how many times a string occurs by counting leaf nodes.

Can find longest repeated substring by searching for deepest node that
has two children.

Suffix links: points to the node which represents the suffix that
started one character after this one started. That is, the node for
`xyz$` points to the node for `yz$`.

Finding the longest common substring of `s1` in the suffix tree for
`s2`. You start matching at start of `s1`. You eventually exhaust. But
then you follow the suffix link to move forward a starting letter in
`s1`.

You want your suffix tree to be a compressed trie. Ukkonen's algorithm
is used to do this. Memory usage appears to be linear.

## Use Cases for Tries

**TODO**: Finish reviewing me!

Autocompletion, T9 autocomplete. Longest prefix is used by IP routing;
how many bytes of the code do you have a match for?

The primary competitor is the BST. A BST lets you find all completions
of a prefix because you can search for the prefix, then just iterate
through forward. But first problem is that you have to keep doing
string comparisons. And you also have to go all the way down the tree
to see if a string is a prefix. And it is not `O(1)` to extend a
prefix by a single letter.

With a trie, lookup is independent of size of dictionary. I guess it's
not totally irrelevant: if you have 1024 keys, then you have a depth
of at 10. So that's a factor of 10 slower than using a trie to lookup
strings (since at each level you have to do a comparison). You might
argue: a trie involves a lot of jumping. But you have to jump a lot in
a BST too.

We know that a suffix tree can be stored in length `O(n)`. And we know
that suffix tree lets us very rapidly find partial matches.

## TODO

These are very old, I'm not sure I'm interested any more...

https://en.wikipedia.org/wiki/Deterministic_acyclic_finite_state_automaton
https://en.wikipedia.org/wiki/Hashed_array_tree
https://en.wikipedia.org/wiki/HAT-trie

## Resources

I don't know if these resources are any good anymore...

https://idea.popcount.org/2012-07-25-introduction-to-hamt/
https://www.cs.cmu.edu/~ckingsf/bioinfo-lectures/suffixtrees.pdf
