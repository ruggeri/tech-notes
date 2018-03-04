## Trie

Sometimes called a *prefix trie* or *prefix tree*.

If storing strings, each edge out corresponds to a letter. If *bitwise
trie*, it's a one or a zero. A trie can store a set or a map.

**Versus Hash Table**

A bitwise trie can be used in place of a hash set or table. It
involves a lot of jumping, though. But in theory `O(m)` time to search
string is equal to `O(m)` time to hash string.

**As A Dictionary**

Every subtree represents all entries with a given prefix. A BST can
basically do this too, though, right? Range query can be used for
prefix.

**Implementation**

You can have a linked list of `(char, pointer)`. This is very wasteful
in terms of pointers. You can have an array for *all* letters:
`pointers[char]` with lots of nulls. That is very wasteful at low
levels of the trie.

You can reduce the waste by reducing the alphabet size: *bitwise*
tries are optimal in that regard. But that means more jumping.

Another approach is a *ternary search tree*. Each vertex has `(symbol,
child, next)`. The `child` extends the prefix, while `next` changes
the last `symbol`.

## Radix Tree

They appear to be "compressed" tries in the sense that each edge can
have multiple characters. If a node has only one child, you can do a
compression. They are sometimes called *compressed tries* or *compact
prefix trees*.

A binary radix tree says that you can have only up to two
children. But a 26-way tree would allow to have many out branches,
reducing depth. The number of ways out is called the *radix*.

I think a bitwise radix tree is called a *Patricia* tree.

A radix tree can use less memory and involve less jumping.

## Ternary Search Tree

As discussed above, is just a way to space-efficiently store children
in a regular prefix trie.

## Hash Tree (persistent data structure)

Just a trie that keys by hash. Sometimes called a *hash trie* which is
a much better name really.

## Hash Array Mapped Trie

A *hash array mapped trie* is a hash trie implementation by
Bagwell. He explains it horribly in his paper. The idea is
simple. Each node consists of a bitmap with one bit per 32
children. It's one if a child is present. There is also an array with
an out-edge per *present* child. This means no nulls must be stored,
and lookup involves a popcount instruction.

An alternative would have been to have an array of out edges, but also
storing the 4bit edge-value. But that would have involved iteration to
find the appropriate out edge to take.

A *ctrie* is a mutable, concurrent version.

## Suffix Tree

Suffix tree for a string `S` is a compressed trie all suffixes stored
in reverse order. Suffixes end in `$` so all values stored at leaf
nodes.

Allows rapidly finding whether a substring. That's because any
substring is the *prefix* of a suffix. Which means that we can just
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

https://en.wikipedia.org/wiki/Deterministic_acyclic_finite_state_automaton
https://en.wikipedia.org/wiki/Hashed_array_tree
https://en.wikipedia.org/wiki/HAT-trie

## Resources

https://idea.popcount.org/2012-07-25-introduction-to-hamt/
https://www.cs.cmu.edu/~ckingsf/bioinfo-lectures/suffixtrees.pdf
