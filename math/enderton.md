# Introduction

Logic will try to answer the questions:

1. What doers it mean for a sentence to "logically follow" from
   another?
2. If a sentence A does logicall follow from B, what methods of proof
   are needed to establish this fact?
3. Is there a gap between what we can prove and what is true?
4. What is the connection between logic and computability? (This is
   a more vague question...).

## Ch0: Facts about sets

Sets are collections of things. Two sets are the same if they have the
same elements. If they are the same, then they have the same elements.

They define a pairs and tuples in terms of sets. `(x, y) = { {x}, {x,
y} }`. Likewise you can define tuples, likewise you can define
finite *sequences*.

Binary relations are sets of ordered pairs. This can be extended to
`n`-ary relations. A function is a relation where the left element
appears only once in the relation.

They give some properties of relations. Reflexivity, symmetry,
transitivity.

There are more. Trichotomy says that for any `x, y`, we have `(x, y)
\in R`, or `(y, x) \in R` *or* `x=y`.

A relation is an *equivalence* relation if it is reflexive, symmetric,
and transitive. It is an *ordering* relation if it satisfies
trichotomy and transitivity.

A set is countable if there is a mapping into the natural
numbers. Whether such a mapping exists is not addressed by the book;
they aren't talking about what sets are definable at this time. They
note that a mapping into `N` implicity *orders* a countable set. They
prove that the set of finite sequences of a countable set is also
countable.

They prove this like so. Take a sequence `a_1, ... a_n`. Map it to
`2**f(a_1) * 3**f(a_2) * ... (nth prime) ** f(a_n)`. This is
one-to-one. They note a technical difficulty: what if `b_1, ..., b_m =
a_1, ... a_n` for `m != n`. This can happen because of how we define
sequences in terms of sets. The worry is that the mapping is not
well-defined because the same sequence can be represented two
different ways.

But they address this difficulty like so. Map each sequence to the
*smallest* number obtainable by the above process.

They mention Zorn's lemma. I won't interpret that right now and will
just wait until it is used.

They mention the cardinality of sets. For finite sets it is the number
of elements. For infinite sets, it is the "least ordinal
number". However, they don't go into that; they basically just say:
the cardinality of two sets is equal if they are *equinumerous* which
means they can be put into one-to-one relation.

If you have two sets A and B, either `A <= B` or `B <= A` or both. (We
say `X <= Y` if X can be put into one-to-one relationship with a
subset of `Y`). Likewise, cardinal numbers can always be compared.

The sum of two cardinal numbers is the cardinality of the union of any
two sets with those cardinalities. Likewise the product of two
cardinal numbers. It can be shown that this is well-defined. Moreover,
it can be shown that for infinite cardinals, both the sum and product
are equal to the *maximum* of the two cardinal numbers.

## Ch1: Sentential Logic

We define a formal language like so:

1. We specify the set of symbols (like parentheses, logical operators,
   and symbols like `A_1`)
2. We specify the rules of the grammar. This defines what finite
   sequences of symbols are *well-formed*. These are called
   *well-formed formulas* or *wff*s.
3. We specify how to translate our wffs into English. This gives the
   language semantics.

You can operate on formal sentences without any knowledge of the
translation into English, though of course the sentences are
meaningless to you...

Okay, let's talk about symbols. When defining our language, we have an
infinite number of them. We assume no symbol is a composition of the
other symbols. (Actually I'm not 100% sure I understand why this is
enough to eliminate ambiguity, but they continue as if that is well
understood. It must be enough, but I don't see it immediately.)

There are symbols that are *sentential connectives*. These plus the
parentheses are *logical symbols*. The "sentence symbols" like `A_42`
are the *parameters*.

Some logicians call `A_42` a "propositional symbol", and call the
logic *propositional logic*.

They define wffs recursively:

1. All propositional symbols are wffs.
2. Negation of a wff, anding/oring of wffs, implication and
   bidirectional implication of wffs are all wffs. (they parenthesize
   each of these, presumably to avoid difficulties with precedence).
3. Every other finite sequence of symbols is *not* a wff.

Basically, all wffs can be built up through a finite process of
formula-building operations. And every wff has a unique parsing.

They note that if you give the formula building operations names like
`e_i`, then every formula is equivalent to a sequence of formula
building operaitons.

The closure of the propositional symbols and the formula building
operations is the set of all wffs. They use this to prove that all
wffs have equal numbers of left and right parens: the formula building
operations always add pairs of parens, and a wff is built by a
sequence of formula building operations, so numerical induction shows
wffs have balanced parens.

## Ch1.2: Truth Assignments

A truth assignment of the propositional symbols is a mapping `S -> {F,
T}`. Let us call this mapping `v`. We want to extend `v` to all wffs.

This is done in the typical recursive way. This extended truth
assignment is unique. To show this, presumably they need to show that
even though a wff can be built up many ways (in terms of ordering of
formula building operations), it all amounts to the same thing at the
end. That is: the parse tree is unique. Note that you can build
expressions in different orders but have the same parse tree. For
instance:

    A
    B
    C
    (A and B)
    (B or C)
    ((A and B) and (B or C))

versus:

    C
    B
    (B or C)
    A
    (A and B)
    ((A and B) and (B or C))

Different order of building, but same result and would parse the
same. This is what they're going to show over the next couple
subsections.

We say an (extended) truth assignment `v` *satisfies* a formula `\phi`
if `v(\phi) = T`.

We say that a set of statements `\Sigma` *tautologically implies*
`\tau` if for every truth assignment that satisfies each `\sigma` also
satisfies `\tau`. We say `\tau` is a *tautology* if `\nullset` implies
`\tau`: that is, `\tau` is true under every truth assignment.

The *compactness theorem* says that for an *infinite* set `\Sigma` of
statements, if every `\Sigma_0` a *finite* subset of `\Sigma` is
satisfied by `v`, then `\Sigma` itself is satisfied. They will prove
this later, and note that it is "not trivial."

**Truth Tables**

Let's see one way to check whether `\Sigma` tautologically implies
`\tau`. (This will work for finite `\Sigma`).

Basically, we build out a table of `2**n` entries, where `n` is the
number of propositional symbols in `\Sigma \union {\tau}`. We then
compute each sentence in `\Sigma` and `\tau` are satisfied by this
truth assignment. If whenever all `\Sigma` are satisfied then `\tau`
is satisfied, then we have established the tautologically implication.

(This appears to rely on the uniqueness of the parse tree, which I
don't believe we have established yet... But whatever, that seems
clear and unsurprising, so maybe they're just waiting to prove it at a
more convenient time.)

This procedure is expoenntial in the number of propositional
symbols. It can be made faster sometimes. For instance, to prove `(A
and (B or C) iff (A and B) or (A and C)`, we can see that if `v(A) =
False`, then regardless of the assignments `v(B)` and `v(C)`, the
statement is true. A key to proving things efficiently is to realize
how to avoid building out the full truth table.

A program that determines whether `\alpha` is a tautology in time
polynomial in the number of symbols is equivalent to `P = NP`. They
don't explain this though, but presumably this is by 3SAT.

# Ch1.3: Parsing

They give a simple parsing algorithm for fully-parenthesized
expressions. The first symbol is always a parenthesis. Read the wff
left to right until you have `(\alpha`, where `\alpha` has balanced
parens. The next symbol must be a logical connective, followed by
`\beta` with balanced parens, then a closing paren.

Extend the parse tree by replacing this node with the connective, a
left child of `\alpha` and a right child of `\beta`. Only mild
exception is negation, in which case you replace `\not \alpha` wiht
`\not` and a single child `\alpha`.

They show that this is actually the *only* way to parse a wff. For
instance, you couldn't put less than `\alpha` as the left child,
because `\alpha` is as short as possible with matching
parens. Likewise, you couldn't put more. Putting more would imply that
an initial prefix of `\alpha'` is `\alpha` a wff. But any proper
prefix of a wff has more left parens than right parens. So `\alpha'`
wouldn't be a wff, so can't be the left child here.

So this shows indeed that a truth assignment `v` on propositional
symbols is uniquely extended to `v\bar` on wffs.

You can ommit parens with Polish notation, which is also syntactically
unambiguous.

## Ch1.4: Induction and Recursion

Given a set of functions `F`, and a base set `B`, a set `S` is
*closed* under `F` if whenever `x, y \in S` then `f_i(x, y) \in S`
(note that F can contain arbitrary n-ary functions). Note that there
is a smallest set `C\star` that is the intersection of all sets `C`
that contain `B` and are closed under `F`. `C\star` is the *closure*
of `B` under the set of functions `F`.

Another approach to construct `C\star` is to start with `B`. Then
extend it to `C_1 = B \union F(B)`, where `F(B)` consists of all
elements of `B` mapped by the `F`. Then repeat and take the union.

These definitions are equivalent.

Recursion is similar. Say that `h(x)` is defined for `x \in B`. We
then show that if `z = f(x, y)`, then `h(z)` is defined in terms of
`h(x)` and `h(y)`. Then we may see that there is at most one
definition of `h` extended to `C` the closure of `B` under `f`.

Note that if the rules are contradictory, there may be no defition of
`f` on all of `C`.

They state and prove the *recursion theorem*. This basically shows
that if the range of the extension function `f` is disjoint from the
base set `B`, and `f` is one-to-one, then there is no potential for
conflict.

## Ch1.5: Sentential Connectives

They ask whether it would extend the expressiveness of the language to
add new connectives. The answer is no. The reason is that how a
connective works is expressed in the truth table.

They define disjunctive normal form, which is a sequence of ors, each
item being ored is a sequence of ands of propositional symbols and
maybe negation symbol.

Since you can write the truth table as an DNF wff, and you can always
express a new connective using just and or and not. That is, you can
always create a tautologically equivalent wff with just ands, ors, and
nots.

Of course not and and are enough to define or. Or just nand can define
not and and.

## Ch1.6: Switching Circuits

This just describes how circuits can be used to implement boolean
logic. We already talked about that in the NAND to Tetris book.

## Ch1.7: Compactness and Effectiveness

They give this statement: if an infinite set of statements `\Sigma`
has every finite subset satisfiable, than `\Sigma` itself is
satisfiable.

I prefer to prove this equivalent (more interesting) corollary: if
`\Sigma` tautologically implies `\tau`, then some finite `\Sigma_0`
tautologically implies `\tau`. This is the first moderately difficult
proof of the book.

**Proof**

(I use "proof" sometimes here, but I really should just say "entails")

So, you say that you need all of `\Sigma` to entail `\tau`. I want to
test this claim. I ask: if I give you `A_1` and you add that to
`\Sigma`, would entail `\tau` be entailed by a finite set of `\Sigma`
plus `A_1`? What I'm asking is: Is `A_1` what you need the "totality"
of `\Sigma` to establish?

You might say: "Yes, knowing `A_1` makes it easy to see that a finite
set entails `\tau`, but that's what I need all of `\Sigma` for." Then,
I ask, what if instead I gave you `\not A_1`? Could you prove `\tau`
with a finite set of `\Sigma` then? Is `\not A_1` what you need all of
`\Sigma` to entail?

You might say again: "Yes. I need all of `\Sigma` to prove `A_1 \or
\not A_1`, and if you give me either `A_1` *or* `\not A_1`, then I
need only a finite set of `\Sigma` now." But this would be a lie: you
already know `A_1 \or \not A_1` as a tautology.

Let me put it another way. If `\Sigma_0 + A_1` entails `\tau`, *and*
`\Sigma_0' + \not A_1` entails `\tau`, then consider `\Sigma_0 +
\Sigma_0'`. That would need to entail `\tau`. In which case, a finite
subset of `\Sigma` *was* enough to entail `\tau`.

So, at least one of `A_1` and `\not A_1` can be added to `\Sigma`
without changing the property that no finite subset of the new
`\Sigma` entails `\tau`. I say: then adding `A_1` (or `\not A_1`, as
the case may be) doesn't change the "difficulty" of the problem: you
still need all of `\Sigma` to prove `\tau`.

Let's note a specific possibility (it's not the only possibility, but
it is illustrative). Say that `\Sigma` entails `A_1` and `\Sigma_0 +
A_1` entails `\tau`. Then `A_1` is like "the heart" of the difficulty
of `\Sigma`s entailment of `\tau`. I'm saying: "let's add `\not
A_1`". That would make `\Sigma` unsatisfiable, which means it entails
`\tau` vacuously. But the difficulty still remains: according to you,
every `\Sigma_0 + \not A_1` is still satisfiable.

So I go through the variables, adding either `A_i` or `\not A_i` to
`\Sigma`. Effectively, I'm one-by-one building a truth-assignment
which satisfies every `\Sigma_0` but will be unable to satisfy
`\Sigma` as a whole.

We can see this, because eventually I add the `{ A_i }` symbols
involved in `\tau`. Because I'm adding symbols such that no finite set
of `\Sigma` entails `\tau`, my setting of the `{ A_i }` must not
satisfy `\tau`. Otherwise, I wouldn't even need any of the original
`\Sigma`, my `{ A_i }` alone would entail `\tau`.

Again: I'm building a truth assignment. The truth assignment by design
will be incompatible with `\tau`. Theoretically, it should be
incompatible with the totality of `\Sigma`. But you keep saying: "Your
truth assignment is compatible with every finite subset of `\Sigma`,
just not the *totality* of `\Sigma`".

So let us take the union of all the `A_i` or `\not A_i` and call this
`A`. This is a full truth assignment. Every `\sigma_i \in \Sigma` is
satisfied by `A`, since otherwise `\sigma_i` plus the `A_i`/`\not A_i`
involved in `\sigma_i` is a finite subset which is unsatisfiable and
thus entailing `\tau`. That is: `\Sigma` is satisfied by the truth
assignment defined by `A`.

But then wait a second. This truth assignment satisfies `\Sigma` but
not `\tau`, which contradicts our original assumption! It is proven!
