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

**Effectiveness and Computability**

They introduce an informal notion of *effective procedure*. It roughly
means exact, finite length instructions that can be executed by a
simple machine. The instructions should terminate after a finite
period of time (though unbounded) and return yes or no.

They want to know: is there an effective procedure to decide if
`\Sigma \implies \tau`.

They define *decidable*. A language `\Sigma` is decidable if there is
an effective procedure to determine whether a given `\tau` is in
`\Sigma`. The language of well-formed formulas is decidable.

Take a finite set `\Sigma`. There is an effective procedure to decide
whether `\tau` is a tautological consequence of `\Sigma`: do the
truth-table thing. Likewise, the set of tautological consequences of
`\Sigma` is decidable. And specifically, the set of tautologies is
decidable.

Now, we know some languages are not decidable. Since programs are
strings, there are countably many of them. But languages are sets of
strings, so there are an uncountable number of them.

If `\Sigma` is infinite, then in general its tautological consequences
are *not decidable*. However, they are *effectively enumerable*: there
is a procedure that lists all tautological consequences of `\Sigma`.

Effectively enumerable clearly means that you have a *semi-decision
procedure*: a program that produces YES if `\tau` is in the language,
but may otherwise run forever. Likewise, if you have a semi-decision
procedure, you can enumerate the members of the language by "time
sharing;" start testing the first statement, and after 1min start
testing the second, then switch to the first again, then try a third,
etc, etc. Thus effectively enumerable languages are exactly those
which are semidecidable.

A decidable language is clearly effectively enumerable. A language
which is semidecidable, and where its complement is semidecidable, is
then decidable (run the two semidecision procedures in parallel; at
least one has to terminate and tell you the answer).

They end with a theorem: for an enumerable set `\Sigma`, the
tautologiacal consequences of `\Sigma` are enumerable. It is easy to
produce a semi-decision procedure for `\tau`. The consequences of any
finite subset of `\Sigma` are decidable. So start with `\nullset`: is
`\tau` implied by `\nullset`? Next is `\tau` implied by `{ \sigma_1
}`. `What about `{ \sigma_1, \sigma_2 }`? Et cetera.

By the compactness theorem, there is a finite subset `\Sigma_0` where
the consequences of `\Sigma_0` are equal to the consequences of all of
`\Sigma`. Thus, in trying to decide whether `\tau` is implied,
eventually we will get to a set large enough to contain `\Sigma_0`, at
which point the truth table method will detect that `\tau` is implied.

On the other hand, if `\tau` is not an implication of `\Sigma`, we
will blow right past our first superset of `\Sigma_0`, not realizing
that it is now hopeless to show that `\tau` is an implication. That's
why this is a *semidecision procedure*.

## Ch2: First-Order Logic

They note that propositional logic is not expressive enough to allow
many deductions that we feel are valid. Let us say we can write our
hypotheses as propositional statements `\Sigma` and our possible
conclusion as `\tau`. If it is true that `\Sigma` implies `\tau`, then
we feel that the conclusion is a valid deduction from the hypotheses.

However, sometimes we cannot make this deduction in the formalism of
propositional logic, even though it appears valid to us. That is: the
formalism of propositional logic is limiting us. Therefore, we
introduce *first-order logic*. A first-order language consists of:

* Parentheses and propositional connective symbols (not and implies
  are all we need to define the others).
* A countable number of variables `v_i`.
* Equality symbol (they say it is *optional*).
* Quantifier symbol (means "for all").
* Predicate symbols: `n`-place predicates. These are like `Hv`. What
  predicate symbols are available depends on the specific first-order
  logic we are talking about.
* Functions symbols.
* Constants (possibly empty). Sometimes these are considered
  "zero-place" function symbols.

Here are some examples of first-order langauges:

1. Pure predicate language
    * No equality symbol; for each `n`, a countable number of n-place
      predicate symbols; a countable number of constants; no functions.
2. Language of Set Theory
    * Equality
    * One predicate symbol: `\element`.
    * No functions, or maybe just `\emptyset`.
3. Elementary Number Theory
    * Equality
    * One predicate: `<`.
    * One constant: `0`.
    * Functions: `S` for successor, `+`, `*`, and `E` (for
      exponentiation).

Since it is generally considered that all mathematics can be written
in terms of set theory (I assume there is no "proving" this), it is
significant that we have a formal language for statements in set
theory.

They now define the syntax. First, *terms* are constants, symbols, and
functions applied to constants and symbols. For instance, in number
theory, `SS0` is a term. An *atomic formula* is a predicate followed
by a number of terms: e.g., `= v_1 v_2`. Last, the wffs are atomic
formulas closed under the following operations: negation (`\not
\alpha`), implication (`\alpha -> \beta`), and quantification
(`\forall v_i \alpha`).

Note, `\not v_1` is not a wff because `v_1` is a term, not an atomic
formula. Though it feels like it should not be a wff because `v_1` is
free. They give an appropriate definnition of *free variable*.

Now, they call a wff without free variables a *sentence*. We're very
primarily intersted in sentences rather than other wffs.

## Ch2.2: Truth and Models

They introduce the concept of a *structure*, which tells us how to
interpret a first-order formula. It defines:

1. The set over which `\forall` quantifies; this is the *universe* or
   *domain* of `A`.
2. For each `n`-place predicate, the structure specifies an `n`-ary
   relation on the domain.
3. For each constant the structure specifies a member of the domain.
4. Likewise, each function symbol is associated to an `n`-ary function
   on the domain to the domain. (Note, we are not allow *partial*
   functions; the functions have domain over the entire set).

So structures tell us how to interpret statements of a
language. Statements in the language are true or false depending on,
whether interpreted in the structure, they are true or false.

For instance, you can use the notation of set theory, and then
interpret is as number theory. In that case, statements true when
interpreted in the structure of set theory may not be true when
interpreted in the structure of number theory.

Let us begin to try to formalize this. Consider a wff. Let `s` be an
assignment of all variables to elements of the domain. Then we can
extend `s` to terms, by replacing variables with their assigned
values, and constants with their corresponding value, and applying
whatever functions are involved.

Likewise, for atomic formulas, we say that a structure `A` satisfies
`= t_1 t_2` with respect to an assignment `s` if `s(t_1) =
s(t_2)`. Likewise for predicates: `P t_1 t_2 t_3` is satisfied by
structure `A` wrt assignment `s` if `(s(t_1), s(t_2), s(t_3))` is a
member of the predicate `P` defined on the domain of `A`. Likewise, we
can extend to negations, implications, and
quantification. Quantification is most interesting. `\forall v_1
\alpha` is satisfied by `A` wrt assignment `s` if `\alpha` is
satisfied wrt `s'` for every `s'` which replaces the assignment of
`v_1` with an element of the domain.

Of course, you don't really need to define all variables with `s`,
only the free variables of the wff. In particular, for a sentence, we
may drop the assignment `s` altogether: either `A` satisfies `\sigma`
for every assignment, or for none. In the first case, we say `\sigma`
is *true* in `A` or that `A` is a *model* of `\sigma`. We can likewise
say that `A` is a model of a set of sentences `\Sigma` if it is a
model of each `\sigma \in \Sigma`.

So structures tell us how to interpret first-order logic wffs. We say
that the structure is a model for the sentences whose interpretation
in the structure is true.

An example of a sentence not true in the structure of rationals but
true in the structure of reals. `\exists y (= (* y y) (+ 1 1))`.

Now we define *logical implication*. We say that `\Sigma` logically
implies `\tau` if for every structure of the language, and for every
assignment `s`, such that the structure satisfies all wffs of `\Sigma`
with `s`, then the structure also satisfies `\tau` with `s`. This is
analogous to tautological implication.

A *valid formula* is the analogoue to a tautology, it is a logical
implication of the empty set.

They list some examples of logical implication. For instance, `\forall
v_1 Qv_1` logically implies `Q v_2`. `\not \not \sigma` is a valid
formula. We prove this by using the "law of double negation". But
isn't that what we are trying to prove?

Meta point. We are proving the law of double negation in the formal
language we are talking about; the formal language is sometimes called
the "object language." We may use any valid reasoning of the
"meta-language" (e.g., English) to do so.

They note that logical implication is much like tautological
implication. But tautological implication was ammenable to a simple
effective procedure to decide tautologies. But to determine logical
implication, you must consider all structures, which are infinite in
number. We will see that the set of valid sentences is not
decidable. (I think this is more like a sneak-peek type note).

They mention the notion of *definability*. A set is definable in a
structure if there is a wff `\phi` with a single free variable such
that an assignment of the free variable satifies `\phi` iff that value
is in the set. For instance, in the structure of real numbers with
multiplication, but not comparison, we can define numbers greater than
or equal to zero by a wff asserting the existence of a square
root. This notion can of course be extended to relations.

Note that not all sets/relations can be definable. For there are an
uncountable number of sets/relations, but only a countable number of
wffs.

Again a sneak peek: they will show that all decidable relations in `N`
(the set of numbers) must be definable in `N` (the structure for the
first order logic).

Now, when we study an area of mathematics, we first use an appropriate
language. Then we throw down some axioms. Now, the study of the field
is to identify what the logical implications are of any model for the
axioms.

Now, they define `Mod \Sigma` be the "class" of all models of
`\Sigma`. This is apparently too large to be a "set", but they don't
go into that particularly. A class of structures is an *elementary
class* if it is the class of models for some sentence `\sigma`. An
*elementary class in the wider sense* is a class of models for a set
`\Sigma`. ("Elementary" is apparently used as a synonym for
"first-order").

Let us give an example. Consider a first-order language with just `=`,
`\forall`, and a binary relation `E`. Some structures for this
language are called *graphs*: the elements of the domain are called
*vertices* and `E` is called the *edge relation*. Caveat: the edge
relation must be symmetric and irreflexive! This is summarized by:

    \forall x (\not xEx) \and (\forall y xEy -> yEx)

So the class of graphs is an *elementary class*. It turns out that
finite graphs are *not* an elementary class, not even in the "wider
sense." They will show this later.

What do these words mean? When we study a field, we pick a language,
and then we restrict ourselves to structures that are models for some
axioms.

Note: a class of models for a finite set of sentences is a simple
elementary class: just take the conjunction of the sentences. We only
need "the wider sense" if there are an infinitude of sentence to
model.

**Read up to homomorphisms**
