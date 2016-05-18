## Propositional Calculus

We start with propositional calculus. Sentences are composed of
variables and logical operators. You can define the language with just
`NOT` and `AND`. We define truth assignment tables.

We can build recursively. Now we can talk about any truth assignments
to any finite statement.

We say a statement is true if every truth assignment is valid.

We can define a bunch of deduction rules. These are a finite number of
finite rules for replacements of symbols. We prove a statement by
applying a series of such reductions.

Of course, the reductions ought to be valid for them to be
useful. That is, the reductions must be true under any truth
assignment. Since they are finite, we can check this, presumably.

So now we have a syntactic definition of what it means for a statement
to be true: it means you can apply these reductions. There is also a
semantic definition of what it means for a statement to be true: given
any setting of the variables, the statement must evaluate to true.

*Soundness* is the idea that everything syntactically true is also
semantically true. That is: if you can prove it, then no one can
produce a setting of variables for the statement that results
false. That is: no such setting exists.

*Completeness* goes the other direction: if it's true, then you can
prove it. That is: if there is no setting of variables that evaluate
the statement false, then a series of transformations should apply.

A system would not be complete if we didn't have sufficient rules of
inference. In that case, we would not be able to prove statements that
really are true.

A system would not be sound if we included rules that weren't really
true. That is, if some setting of variables gave a different result
*before* and *after* the transformation.

You can prove soundness pretty easily; I think it is just a matter of
proving the original rules sound, then extending via induction.

**Completeness**

To prove completeness (of propositional calculus), I think is a little
tougher. Let's consider the contrapositive: show that if the system
doesn't prove P, then P is not semantically true.

In that case, consider a maximal set of rules R\*, where R\* still
does not prove P. We need to show we can construct this set: we can do
that by ordering the sentences in the language. Then for a sentence
`i`, let `R_i` be `R_{i-1}`, plus also maybe sentence `i` if the
statement is still not implied by the system.

Then you are allowed to take the countable union of the `R_i` to
produce `R*`. We can show this still doesn't prove `P`.

**TODO**: Finish this. Not hyper-interested right now.

## First-Order Logic

Here we extend our language with the forall symbol.

We need to prove this system sound and complete. **TODO**

## Sources

* Enderton Elements of Set Theory was the book I used, I think.

## Kolmogorov Complexity

Idea is that the *information content* of a string is the length of
the shortest program that produces that string. Obviously that is
relative to a description language. We typically require the language
to be Turing complete.

How can we compare the K-complexity of a string in a language L1 vs
another language L2? Well, L1 can simulate L2; a TM in L1 describes
how to run a program described in L2. So, at a maximum, the
K-complexity in L1 is the description length in L2, plus a constant:
the length of the shortest program to interpret L2. This is called the
*invariance theorem*.

Kolmogorov was interested in declaring strings *random*; which means
that the K-complexity is greater than the length of the string in an
Turing complete language. But that doesn't really make sense to me,
there is always a TM of length 1 which outputs a given finite string:
have the first bit be 1 to output the given string, and then suffix a
regular language's encoding of a normal program.

I think the trick is this: finite strings are never considered
random. An infinite sequence of characters can be considered
random. If we map each finite prefix `S_i` to it's K-complexity
`k(S_i)`, then we say the sequence is random if this is unbounded. In
that case, no finite algorithm could be producing this data.

You might try to strengthen this. Imagine I took a random sequence,
and then transformed it like this: repeat the first random character
once, the second two times, the third four timex, the fourth eight
times... I'm sort of spreading the entropy out. You might say that
this is "less random". I think to be truly random, you're supposed to
say that the k-Complexity grows linearly.

Ray Solomonof was trying to find a universal prior for Bayesian
inference. We know that, to do statistical reasoning, we need to
either (1) impose arbitrary rules in advance (e.g., that the
relationship between variables is linear), or (2) start with a prior
distribution and work forward.

Of course, the prior distribution is just as arbitrary a rule.

Solomonof wanted to address that. He suggested something like this (I
think). Consider all universal TMs. Give all these machines equal
probability. Something something something...
