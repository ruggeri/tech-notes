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
