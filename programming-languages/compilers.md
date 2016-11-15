# Ch 1: Introduction

* Compiler translates a source language to a target language.
* Can emit machine code, bytecode. Bytecode can be JITed by the
  runtime.
* Front-End consists of:
    * Lexical Analyzer (AKA lexer or scanner)
        * Basically tokenizes input.
    * Syntax Analyzer
        * Builds syntax tree, verifies code is syntactically correct.
    * Semantic Analyzer
        * Makes sure that program makes sense semantically.
        * E.g. does type checking.
    * Intermediate Code Generator
        * Converts to an intermediate format like SSA.
* Back-End:
    * Does machine independent optimization.
    * Generates machine code.
    * Does machine-dependent optimizations.
* Other parts:
    * Compiler may generate assembly language code; the assembler
      converts this to relocatable machine code.
    * Linker links in other relocatable object files.
* They talk about the register keyword in C. This used to be necessary
  when the compiler didn't know how to allocate registers well, but
  now often the compiler can do better. By using the register keyword,
  you may prevent the compiler from making useful optimizations.
* Inlining of methods. Virtual method dispatch optimizations.
* To take advantage of parallelism, first you want to take advantage
  of pipelining and out-of-order execution.
    * This is done by the HW, but you can play nice with this.
    * VLIW allows you to do this explicitly.
    * Vectorized instructions.
* Automatic parallelization to multiple threads (doesn't typically
  happen).
* Play nice with the memory hierarchy. Used to concentrate on CPU, but
  now since the CPU is starved for data, caching is most important.
* CISC => RISC
    * CISC was handy for programmers so they could have a wider
      variety of instructions.
    * But compilers don't care. And they'll often avoid a CISC command
      if they don't need all of that heavyweight command.
    * And I think compilers aren't good at taking advantage of CISC
      anyway.
* Other Uses
    * Binary translation: from one machine code to another.
    * Hardware synthesis: compile combinatorial logic to gates to
      transistors. Optimization can take hours!
    * SQL queries get compiled.

# Ch 2: Syntax Directed Translator

* Syntax is defined as context-free grammar. Notation is called BNF.
* Each "rule" is called a *production*. *Nonterminals* are sort of
  like types, and they have rules translating them into *terminals*
  (characters) and other nonterminals.
* BTW, a context-free grammar means are rules are like `A -> abc`.
    * A *language* is a set of strings.
    * The language is *recognizable* if a TM exists that will return
      true for any input from the language, but may run forever for an
      input not from the language (if it halts it must return false).
    * Recognizable turns out to be the same as *enumerable*: can a TM
      print every string of the language?
        * To go from enumeration to recognition: given an input,
          enumerate the language strings. Does the input match any?
        * Other way: iterate through all strings. Run the recognizer
          on each and print if recognized.
        * Because recognizer can loop forever, you'll need to
          "timeslice" by juggling multiple TM executions. Run one step
          of all current TMs, then move to the next string and start a
          new TM. Repeat. Print whenever you recognize a string.
    * Decidable means there exists a TM to both *accept* or *reject*.
    * A grammar is a set of rules with left and right sides called
      *productions*.
    * All decidable languages can be expressed by a finite number of
      production rules. (Says wikipedia, I didn't prove this).
    * Context-free grammar says that all rules of the `X -> abc`. That
      is, the left side is always a single class.
    * Context-sensitive grammars say that rules of the form `aXb ->
      ayb`. That is, the rule can depend not just on the nonterminal
      class, but also characters before/after.
    * Regular expressions are even more restrictive than context-free.
* Given a grammar, we want to transform a string into a *parse
  tree*. This is *parsing*.
    * Our rules must not be ambiguous, otherwise there may be
      multiple ways to parse it.
    * I think parsing ambiguity is still technically a "grammar", but
      we would never accept such a grammar for our purposes.
    * I believe this is also called a *concrete syntax tree*.
* Must nail down associativity and precedence of arithmetic operators
  to avoid ambiguity.
* Syntax-Directed Translation. *Syntax-Directed* means that code
  fragments are associated with production rules. Whenever you use
  that rule when parsing, you run the code.
* They show syntax-directed rules for translating infix to postfix.
* Okay, but how do you parse? *Top down* parsers start at the root
  nonterminal and try to work their way down to the leaves. They can
  be written efficiently by hand. *Bottom up* parsers can handle a
  larger class of grammars; they are typically autogenerated. We'll
  focus on top-down for this chapter.
* You want to be able to do a simple left-to-right scan of the string
  for your parser. You'd want the next `k` characters to determine
  what rule to apply, and then you just move on. That kind of parser
  is called a *predictive parser*.
    * It won't work for just any grammar. These grammars are called
      `LL(k)` (apparently). The `k` is how many letters needed.
    * Predictive parser means no backtracking, but also means (1) no
      ambiguity in parsing and more importantly (2) no left recursion.
    * Predictive parser runs in linear time.
* A *recursive descent parser* (of which predictive parsers are one
  type) has procedures for each production rule, and calls deeper and
  deeper into these to match the string further.
* You can eliminate left recursion (at least some of the time) via `A
  -> Aa | b` to `A -> aR` and `R -> aR | b`.
* After parsing completes, we often transform the *parse tree* (or
  *concrete syntax tree*) to an *abstract syntax tree* (often just
  *syntax tree*).
    * This basically just loses some of the excessive information of
      parsing.
    * It's supposed to just keep semantically-useful information. It
      doesn't need all the nodes for syntax.
    * E.g., if we made nodes for terminals like `(` and `)` in the
      parse tree, those won't be needed in the syntax tree.
* Translation from parse to syntax tree can be syntax-directed, of
  course.
* Before we run the parser, we'll make its job easy with a lexer.
    * This just tries to find tokens. The grammar the syntax analyzer
      will try to build a parse tree out of is written in terms of
      tokens.
    * Tokens can be keywords, or they can have a class like number or
      id. A number would have an attribute for its value and an id
      would have an attribute for its name.
    * The syntax analyzer doesn't care about the value/name, though
      these will of course be used later.
    * Obviously whitespace and comments can be removed by the lexer.
    * Lexer is typically easy to write and just looks ahead maybe a
      letter for a space.
* Symbol table is a table in which we can associate symbols with
  attributes. It's a hash-map basically.
    * Except to deal with scope, it's a *nested* hash-map.
* If the code parses, then when we turn it into an AST, we typically
  check semantic rules here. E.g., if we're adding two variables, do
  they have appropriate types?
* *Three address code* is like *static single assignment*. It's an
  intermediate representation (*IR*) used on the way to generating
  assembly. It should hopefully be easy to optimize.
    * It also needs instruction for conditional and unconditional
      jump.
    * They show a syntax directed translation to three address code.

## Ch 3: Lexical Analysis

* Lexer can be autogenerated from rules (Lex).
* Can run pipelined with the parser.
* Scanning is simple stuff like comment removal and whitespace
  removal, lexical analysis produces tokens.
* Examples of tokens include:
    * if, else, while, for
    * binary operators (comparison operators, e.g.)
    * id (would record identifier name as an attribute)
    * number (would record value as attribute)
    * string literal
    * assignment operator
* Regexp specify languages.
    * The basis is empty string plus the letters of the alphabet.
    * Then close over `(x|y)`, `xy`, `x*`.
* A regular language is that which is matched by regular expressions.
* Token classes are often defined in terms of regex. For reserved
  words, we use a literal string. For numbers, we match digits, with
  an optional fractional part and an optional exponent part.
* So it's easy to use a DFA to see if strings match.
* One approach to combine DFAs for multiple token classes is to:
    * Start matching the string.
    * Advance in each DFA.
    * As you go, token classes will be eliminated when you encounter
      letters for which there is no transition (that class doesn't
      match).
    * Continue until you eliminate the last DFA.
    * At this point, go back to the previous longest match.
    * If there are ties, the token classes should be prioritized
      (e.g., reserved keywords over identifiers).
* Lex
    * You declare token classes and regexp.
    * You optionally provide C code to run when the class matches.
    * Lex exposes some globals: `yylval`, which is where you can store
      any attributes (or pointer to struct with attributes).
    * It also gives you `yytext` and `yylen` the text (and its length)
      which matched.
* Lex takes this source program and compiles it to C code. This can
  then either be linked with your program, or it can run as a
  standalone program.
    * I think it's meant to be an input to the `Yacc` parser
      generator.
* The `/` operator is a "lookahead" operator. It says: "the stuff to
  the left was the token, but it must be followed by the stuff to the
  right, which is not part of the token."
* Finite Automata:
    * DFA is pretty easy.
    * NFA basically allows you to transition to multiple
      states. You're in the union of the states.
* Computability sidenote:
    * NFA can always be simulated by a DFA. Basically, the DFA keeps
      track of the set of states the NFA could be in.
    * So NFA does not increase the computability power of DFA.
    * Let's considered a NTM, a non-deterministic Turing Machine.
    * It can likewise be simulated by a TM.
* Complexity Sidenote:
    * Problems that an NTM can solve in polytime are exactly those
      that a TM can verify in polytime.
    * NTM => verifier is easy. Record the sequence of choices the NTM
      makes as a string. Then have the TM run the NTM program, but
      always make the specified choice along the way.
    * Verifier => NTM. This feels cheap. Have the NTM guess each
      character of the certificate. Then run the verifier.
* Can represent automata with a transition table of `(state,
  character) -> states`.
* We can convert an NFA to a DFA.
    * Each DFA state represents possibly multiple NFA states.
    * You start with a DFA state for the start NFA state.
    * Iterate through the NFA states mapped to the DFA state.
    * Iterate through the characters transitioning out of the NFA state.
    * If you haven't seen this char before for this DFA state, create
      a new DFA state.
    * Record the NFA states that have to be in this DFA state.
    * Continue.
* Alternatively, you can just "simulate" an NFA by doing the DFA
  translation "on-the-fly".
    * Basically, you just keep track of all the states you could be
      in, then update each time you process another character.
    * You just iterate through current states, looking for transitions
      with the current character. Add this to a set of new states.
    * To avoid adding the same state multiple times, you can keep a
      bitstring representing whether a state has been added to the
      collection of new states.
    * Before pushing a new state on, make sure to take its epsilon
      closure (by repeatedly trying to transition from that state with
      empty string).
    * Time complexity is given by:
        * Assume `n` states and `m` transitions.
        * So there are `n` states max on the stack to iterate through.
        * Now we have to check the transitions out of these states for
          each character (plus empty string transitions).
        * This is of course bounded by `m`, so there is `O(n+m)` work
          to do for each character.
        * So `O(k(n+m))` to process `k` characters.
* Construction of NFA from a regex.
    * Simple to build a NFA for a single character `l`.
    * To build one for `s|t`, take the NFA for `s` and one for `t` and
      join them in parallel from a start using empty string
      transitions. Joing their accept states to a final accept state
      with an empty transition.
    * To build an NFA for `st`, just wire in series.
    * Last, to build an NFA for `s*`, add a empty string loop from the
      end to the start. Also need to add an empty transition from
      start to end.
* So NFA construction means that we are joining NFAs together.
    * Each combination adds at most two new states and up to four
      transitions.
    * So the number of states and transitions is `O(r)`, the length of
      the regex.
    * Therefore, construction is `O(r)`.
    * Since NFA simulation is `O(x*(m+n))`, this means `O(x*r)`.
    * (NB: We have elided *parsing* of the regex, which can be done in
      linear time, though this will have to be proven later).
* NFA simulation thus requires ideal setup time, but can be somewhat
  slow.
* If the regex will be used repeatedly (as in lexers for compilers),
  it may justify more initial work to build a DFA, since DFA
  simulation is linear in the length of the string.
    * This will not work if there are too many DFA states
* The conversion will be faster for repeated reuse, but the simulation
  could be faster for a single use. Another problem: the transition
  table of the DFA may require a ton of memory, because you may create
  a ton of new DFA states.
    * That will not only result in a lot of initial work, but also
      will have bad performance per string because the transition
      table won't fit in cache or maybe even memory.
* In the worst case, a translation from NFA to DFA may involve `2**n`
  DFA states: one for each subset of `n` NFA states.
* Let's envision how we can hit this pathalogical case. Say that the
  finite automata is `(a|b)*a(a|b)**(n-1)`. The straightforward NFA
  would have a loop for `a|b` at the beginning, followed by an `a`
  transition, then `n-1` transitions of `a|b`.
    * Let's show we need at least `2**n` DFA states.
    * Keeping track of the values of the last `n` characters would
      require `2**n` DFA states.
    * This would be enough to solve.
    * Let's say we forgot whether any position `k` started with an `a`
      or a `b`. Then say that subsequently we got the a's and b's we
      need.
    * How do we know whether to accept or reject?
    * Very handwavy, but I think I'm satisfied.
* So how long to convert from NFA to DFA?
    * We know the length of the NFA is `O(r)` states. We know the
      number of transitions is `O(r)`.
    * We know that each DFA state corresponds to up to `O(r)` NFA
      states (all of them). We need to consider all possible
      transitions out (`O(r)`). So this is `O(r**2)` work per DFA
      state.
* In a common case, the number of DFA is linear in the regex size. So
  the compilation of the DFA is `O(r**3)`. But of course it could be
  as bad as `O(r**2 * 2**r)`.
* They give an algorithm for direct DFA construction from a regex.
    * First you parse the regex.
    * Each (non empty) operand in the regex is given a position.
    * Each node in the parse tree gets values for `firstpos(node)` and
      `lastpos(node)`. These are sets of operand
      positions. `firstpos(node)` means, for the regex represented by
      this node, what operands could match the first character of an
      accepted string. Likewise, `lastpos` means the set of positions
      where a matched string could end at.
    * `nullable(node)` is true or false: could the empty string match
      this regex?
    * It's easy to recursively build these values. For instance,
      `firstpos(s|r)` is the union of `firstpos(s)` and `firstpos(r)`.
    * I won't give the rules for `sr` and `s*`. The basis is
      `firstpos(\eps)=\nullset` and `firstpos(c_i)=i`.
* Now we'll start to talk about a property called
  `followpos(c_i)`. This is the set of possible operands positions
  that could be the next operand matching after `c_i` in the full
  regex.
    * You can build this from the previously calculated quantities.
    * To calculate for a node, start walking up its ancestors.
    * If you're on the left of a concatenation operation, add
      `firstpos(right)` to your followpos set. Stop.
    * If you encounter a star, add `firstpos` of this node. Keep going
      up.
* You could easily create an NFA from this. Create a state per
  operand. Then create a transition from one operand to the next in
  `followpos` (one character may exit to multiple operands).
    * Make all positions in `firstpos` of the root be initial states.
    * To make a single state mean acceptance, you should just
      concatenate a special `#` symbol at the end of the regex. This
      is the only acceptance state.
* Or you can generate a DFA directly.
    * The start state is `firstpos(parse tree root)`.
    * Now start trying to "grow" new states from the start state (call
      it `S`).
    * Consider each symbol `a`. Consider each `pos` in `S`
      where`c_pos=a`. Now let `U` be the union of
      `followpos(pos)`. These are the potential next letters to match
      if we see an `a`.
    * If a state for `U` doesn't exist, add it!
    * After considering all symbols, we've figured out all transitions
      from `S` to new sets of states. We can "mark" `S` as done.
    * But we should continue with created states, further extending
      these.
    * The accepting state is the one that contains the `#` symbol as a
      next position.
* Note to self: regular expressions, even though very limited in terms
  of languages they can accept, lead to ambiguous parsing. For
  instance `aa*`.
* State minimization can reduce redundant states in a DFA. I will
  sketch the approach:
    * Partition DFA states into accepting and not accepting.
    * Break down each partition by consider where each state can
      transition to. Even if two states `s1` and `s2` can transition
      to different new states, keep them together if `s1` and `s2`
      both transition to new states *in the same partition*.
    * In that case, `s1` and `s2` are practically indistinguishable
      for acceptance purposes (so far).
    * Continue until partitions stabilize. Note that you might not
      break up a partition in one round, but may in a subsequent
      round.
    * When done, you can just pick representatives of each member of
      the partition.
* The book mentions (without proof) that a minimal state DFA achieves
  the minimum number of states possible. That is, we can always reduce
  any DFA to a "best" DFA. So our state minimization algorithm
  achieves the best DFA.
    * The smaller the # of states, the more cache friendly, of course.
* A little care is needed when acceptance states mean different
  things. For instance, in a lexer, there's a regex for each token,
  and different acceptance states means different tokens match.
    * So our partitions should be non-acceptance states, plus each set
      of acceptance states for a given set of regexs.

## Ch4: Syntax Analysis

* Transform source code to parse tree.
* Two major methods are top-down and bottom-up parsing. Another is
  *universal* parsing, which can parse any context-free grammar, but
  is slow. Top-down and bottom-up can only parse subsets of CFG.
* Syntactic errors can be detected by the parser, but semantic and
  logical errors must be detected later.
    * Will want to report where the error is.
    * And also will want to "recover", be able to continue parsing, to
      generate more error messages.
    * However, I'm not very interested in this.
* A CFG has production has productions with a head and tail, and the
  head is just a non-terminal; it doesn't have multiple symbols.
* Derivations is a sequence of strings, starting from the start symbol
  and replacing with tails of productions.
    * Leftmost derivations replace leftmost nonterminal at each step.
    * Rightmost derivations replae rightmost nonterminal at each step.
    * Anysequence of terminals and nonterminals that can be derived
      from the start symbol is a *sentential form*. If it contains
      only terminals it is a *sentence*.
    * Can have many derivations, but still only one parse tree.
* CFG is is more expressive than regular expressions. You can easily
  rewrite any regular expression as a context free grammar.
    * More than this: we can rewrite any NFA as a context free
      grammar.
    * There's a non-terminal for each state. There are productions for
      every transition from one state to the next.
    * At the same time, no regular expression can match `{a**n b**n |
      n >= 1}`. This is trivial with CFG.
* Seperating the lexical makes it easier to write a parser. Also,
  since lexer is typically a regexp. And then grammar can be of a form
  to allow a faster parsing strategy.
* Grammar can be ambiguous. For instance `if E then S` and `if E then
  S else S`.
* Left recursion can make parsing hard. You can always *factor*
  productions to eliminate left recursion.
* Some language constructs, though logical, can not be expressed with
  a CFG.
    * For instance, can't check declaration of identifiers before
      their use.
    * Also can't check the number of parameters to a function.
* Top-down parsing: starts from the start symbol and works down.
* Recursive descent is a general strategy, but may require
  backtracking.
* `first(\alpha)` means the set of terminals that can start
  `\alpha`. `follow(\alpha)` means the set of terminals that can
  follow `\alpha`.
    * We can compute `first(X)` by repeated substitution.
    * We can compute `follow(X)` by considering `first(Y)` for
      anything where `A -> ...XY...`.
    * That's a very loose description of how to calculate, but I'm not
      trying to get too involved with these definitions.
* `LL(1)` grammars require one character of lookahead to compute a
  leftmost derivation without backtracking.
    * Need `A -> \alpha | \beta`, `first(\alpha) \intersect
      first(\beta) = \empty`.
    * Also, if either `\alpha` or `\beta` can be the empty string,
      need to check that `follow(\alpha)` has no intersection with
      `first(\beta)`.
* To produce a *predictive parsing table*, for every production `A ->
  \alpha`, iterate through `a \in first(\alpha)`, setting `M[A, a]`.
* You don't need to do recursion to predictive parsing. You can
  maintain the stack explicitly yourself.
* According to wikipedia, `LL(k)` is typically not used, because the
  parse table increases in size exponentially in `k`. There is also a
  notion of `LL(*)`, where we use a DFA to make parsing decisions, not
  just look at a symbol table. Note that this may require an unlimited
  lookahead.
    * These appear to be speciality subjects, and are not covered in
      the text.
* Bottom-up parsing starts from the bottom and works its way
  up. Basically, we produce a "backward derivation". The steps
  backward are called *reductions*.
* Consider a rightmost derivation. Say that `\alphaAw => \alpha\betaw`
  (`w` are terminals). That is, we use the production `A -> \beta` to
  replace `A`. Then, given `\alpha\betaw`, the *handle* is defined to
  be `\beta`.
* We can obtain a rightmost derivation in reverse by repeatdly *handle
  pruning*. We identify the handle, and then replcae it with the head
  of the production.
* This is the basic idea of *shift-reduce parsing*. Basically, we'll
  scan left to right through the string. We *shift* characters of
  input, pushing them on a stack. When we can identify a handle, we'll
  *reduce* it, by replacing it with the left-hand side of the
  production.
* LR Parsers do left-to-right scanning, producing a rightmost
  derivation in reverse. `LR(0)` and `LR(1)` are the most interesting
  cases.
* There are several ways to construct shift reduce parsers. These
  include simple LR (SLR), canonical LR, and LALR.
    * These differ in how we produce states and parsing table.
* LR parsers can handle all languages we care about. It is a superset
  of LL.
    * TODO: can I prove that?
* LR parsers are too much work to construct by hand, so we
  autogenerate them.
* So how do LR parsers work? When do we shift and when reduce?
* The states of an LR parser are sets of *items*, which are
  productions, and our progress through them. For instance, `A -> XY`
  yields three *items*: `A -> *XY`, `A -> X*Y`, and `A -> XY*`. It's
  about how much of the production we've matched.
* We define `closure(I)`. If `A -> \alpha *B \beta` is in `I`, then we
  add `B -> * \gamma` to `I`, until this can be done no more.
    * Assume we have `I=closure(I)`.
    * The *kernel* of `I` are those items we've matched part of (plus
      the initial item, which is `S' -> *S`, where `S` is the start
      symbol and `S'` is a synthetic "top-level" symbol).
    * The nonkernel items are where the dot is on the left.
    * Nonkernel items are always derived from kernel items.
* `goto(I, X)`, where `X` is a symbol, means we consider all elements
  of `I` where `A -> \alpha * X \beta`. We then set `goto(I,X)` to be
  the closure of `A -> \alpha X * \beta`.
* So the idea is: a closed set `I` is the current state. At the
  beginning, it's the closure of `S' -> S`. Each character you see,
  you either *shift*, in which case you use `GOTO(I, a)`, which is a
  transition in the parse table. You push this state on a stack.
* Eventually, `GOTO(I, a)` will be empty. That is, the corresponding
  entry in the parse table is NULL. That means a handle has finished
  forming (or there's a parse error).
    * For this current state, there should be exactly one member of
      `I`, say, `A->XYZ*` which you've finished producing.
    * Pop the current state. Shift `A` now as if it were a terminal
      being shifted.
* Eventually you'll finish reducing. At this point, you can indeed
  shift the next terminal.
* How to construct the SLR parsing table:
    * I don't feel like they tell you how to construct the SLR table.
    * It sounds like maybe you start with the initial state, then just
      try each terminal, and calculate `GOTO(I, a)`.
    * In fact that's exactly what they tell you to do.
    * Note that `GOTO(I, a)` is as simple as iterating through `I`,
      provided you have done the closing over `I`.
* The `LR(0)` automaton has these states and the given transitions.
* We can construct a table, called the `SLR(1)` table, which has the
  action to perform for each terminal. It is either a *shift*, to add
  another state, or a *reduce* to hold onto the terminal a moment, and
  then produces a new non-terminal.
* There are unambiguous grammars which cannot be SLR(1). Let's
  consider a failure.
    * `S -> L=R | R`
    * `L -> *R | id`
    * `R -> L`.
    * This is unambiguous. However, consider the state where we are in
      the initial state and see an `L`. Then we must transition to a
      state with the following items: `S -> L\cdot = R` and `R ->
      L\cdot`.
    * Then we see an `=`. Of course, the right thing to do is to shift
      tp a new state with the kernel `S -> L = \cdot R`.
    * But the problem is we could also see...
