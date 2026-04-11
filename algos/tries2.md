# Tries

Sometimes called a **prefix trie** or **prefix tree**.

If storing strings, each edge out corresponds to a next letter. If
bitwise trie, it's a one or a zero. A trie can store a set or a map.

**Implementation: Successor Nodes Array**

A naive implementation allocates an array of node pointers with length
equal to the size of the symbol space. That allows the quickest lookup
of the successor; you index into the array by the successor token, and
then go to that node (or identify it as NULL).

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

## Radix Tree

**TODO**: Finish reviewing me!

They appear to be "compressed" tries in the sense that each edge can
have multiple characters. If a node has only one child, you can do a
compression. They are sometimes called _compressed tries_ or _compact
prefix trees_.

A binary radix tree says that you can have only up to two
children. But a 26-way tree would allow to have many out branches,
reducing depth. The number of ways out is called the _radix_.

I think a bitwise radix tree is called a _Patricia_ tree.

A radix tree can use less memory and involve less jumping.

- This is a space-compressed version where the edges are strings,
  representing the letters that strings have in common.
- This is sometimes also called a PATRICIA tree when the radix is 2,
  meaning that you compare by bit.

## Ternary Search Tree

**TODO**: Finish reviewing me!

Another approach is a _ternary search tree_. Each vertex has `(symbol,
child, next)`. The `child` extends the prefix, while `next` changes
the last `symbol`.

As discussed above, is just a way to space-efficiently store children
in a regular prefix trie.

Another common approach is a _ternary search tree_. Here, you have
left and right to search for a different letter at this position, and
middle path for extending the word. This stores fewer pointers when
the extensions are sparse.

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
