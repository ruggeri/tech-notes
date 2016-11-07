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
