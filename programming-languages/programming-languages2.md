Link:

    https://docs.google.com/spreadsheets/d/1cCgNwxc8TNRkVAA5u8-_GMef6-vlKoBDasBvhOm88DE/edit#gid=0

## Results Summary

For scripting or fast-iteration web-development, prefer
Python/Ruby. You could use JS, but increased performance probably
doesn't justify the increased pain.

For performance critical work, use Rust. Rust gives you the
appropriate level of control, while providing much more safety. Use
Rust for projects where performance will be the difference between
success/failure. Rustc is apparently getting much faster, too, leaving
ever less oxygen for Golang.

For performance-sensitive prototyping, consider Clojure. In
particular: is there a high probability the work will be thrown away?
If perf doesn't matter, of course use Python/Ruby. I'm thinking of
AI/ML stuff like at QC. Development can still be painful when problems
a compiler would have detected waste time. Iteration cycle for this
kind of work is often longer, so tests are more important. Not *that*
much space here.

For large app development meant to last a longtime, you want static
typing. The primary goal is to enable later refactoring. You still
want as rapid development as you can still get, which is your
challenge. It is a battle between Golang and Scala; each has its
weaknesses, and it's unclear what I would like better without more
experience. My guess is that I would prefer Scala despite its
compilation woes because it is more expressive. But I would have to
try that out.

## Values

Dimensions I care about:

* Safety: Can the program verify that it is working, at least to a
  basic level. Is it easy to keep bugs from coming in?
    * Also: safety versus "debugability". Can you identify an error
      quickly?
* Performance: is the program fast? Is it easy to optimize?
* Ease/Speed of development: can you build programs quickly?

Categories:

* C/C++: no sacrifice of performance, but easy to make memory
  management mistakes, or overrun mistakes.
* C#/Java: eliminates all memory errors, but often verbose typing
  information. Hard to get real low level.
* Ruby/Python: very easy development, but no multithreading. Fast code
  needs to be written elsewhere. Lack of type information means it can
  be hard to figure out what code does.
    * Lua/Perl/Groovy are in this category too.
    * Javascript is also in this category, but even more dangerous and
      annoying to debug. TypeScript addresses some of those flaws,
      though, I guess.
* Ocaml/Haskell/F#/Scala: functional, emphasize purity. Functional is
  a cool, different way of thinking about control-flow. Cool,
  expressive types. But can be a lot of compiler fighting; I think
  this kind of safety is over-the-top. Scala can also be really
  cryptic. Performance isn't emphasized.
    * Also, it's often feels painful if you don't 100% adopt the style
      of functional programming. I default to thinking in imerative
      "steps", rather than a series of pure transformations on
      data. But then you have to hack everything into monads...
* Common Lisp/Scheme/Clojure: all about macros. Supposed to be very
  fluid. But lack of imperative control can also feel limiting. Hard
  to feel efficient.
* Erlang/Elixir: all about actors/message passing. I really doubt
  that's a feature I want a language built around. Sounds more like a
  library (like Akka).
* Objective-C/Swift: a different take on C++ or maybe Scala. A little
  more flexibility than C++, but less performance. Still have a lot of
  legacy cruft. Really only used in one domain.
* Rust: the new C++. Can be hard to escape the strictures of memory
  management. But very safe without the tradeoffs of Java/C#, and also
  incorporates some nice ideas from Haskell.
* Golang: sort of an alternative to Java. Builds in this concurrency
  primitive. But then is somewhat obnoxiously untyped, has weird
  manipulation of arrays.
* Elm

Summary:

I do like types, because they are built in documentation. I need a
strong ecosystem with lots of libraries. I want a minimum level of
safety to prevent me wasting time debugging memory problems.

I like having a REPL, or being able to write scripts, because that
allows me to understand how code works fast.

I feel like I need to know the bottom-most representation of
things. Abstraction can be very frustrating.

## To Explore

* Elm
    * I don't know much about it. It is a different paradigm, so that
      could be fun to try.
* Play framework for Scala.
* Idris
    * Like Haskell, but with *dependent types*, which allows for more
      information to be associated with a variable or return type,
      which allows the compiler to check and verify more sophisticated
      invariants.
