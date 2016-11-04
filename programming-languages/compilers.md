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
