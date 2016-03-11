Simplest kind of trie uses *direct addressing*: for every character in
the set, you have an entry. When the set is large, this can be
wasteful.At the start of the trie, you often have pretty dense usage,
but toward the middle it thins out. If there are many suffixes, you
may have a higher branching factor at the end again. But in the middle
part, you have all these addresses, but little usage: the effective
branching factor is low.

You can try to address the problem by viewing strings as longer
strings over a smaller alphabet. For instance, you can do your trie
bit-by-bit. That makes it effectively a binary trie. The problem is
that now you have a deeper trie and more jumping.

One way to solve this is to use a BST inside your trie node, so that
you don't need to represent every character. Your BST may look at each
bit in the current character. TST has problem of having logarithmic
search for character (in size of character set). In addition, are you
going to do self-balancing of that BST? That sounds complicated and
expensive.

How do you label things as present? You could put a "present?" field
in each node. Or, in your branch table, you could have two kinds of
entries: (1) pointer to a next node, or (2) pointer to a value, if
that's the only value with that prefix. This avoids unnecessary extra
nodes when there is only one completion anyway, though it probably add
some "splitting" cost.

Okay: so you don't want a tree, and you don't want direct
accessing. One way to do this is to keep a list of successor nodes,
but also a bitmask of length cardinality of the set, with a `1` iff
that character has a successor. This is very compact. It turns out,
with just a few operations, it is easy to find the index of a given
character. The necessary bitwise operations are often implemented in
hardware by the CPU. This technique is called "compression" and was
invented by Bagwell. I believe this is the "array mapping" in
hash-array mapped trie.

Insertion requires you to rewrite the array. That sounds like it would
cause allocation pressure, but I guess not? That is *exactly* what
PersistentHashMap does in clojure.

This is called an AMT.

## Ternary Search Tree

Another common approach is a *ternary search tree*. Here, you have
left and right to search for a different letter at this position, and
middle path for extending the word. This stores fewer pointers when
the extensions are sparse.
