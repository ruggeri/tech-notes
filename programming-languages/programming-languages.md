## All the langauges!

From http://githut.info. I removed some that aren't relevant. I also
looked at TIOBE and Github's lists of PLs to see if I missed
anyone. *I didn't*.

* *These languages have >20k active repos*
* <del>JavaScript (1.9MM, 3,881 jobs)</del> (Not easy to parallelize; would prefer Python/Ruby)
* <del>Java (1.7MM, 5,739 jobs)</del> (Much boilerplate)
* <del>Python (959k, 4,335 jobs)</del> (Not easy to parallelize)
* <del>Ruby (965k, 2,207 jobs)</del> (Not easy to parallelize)
* <del>C++ (500k, 1,989 jobs)</del> (Too unsafe)
* <del>C (450k)</del> (Too unsafe)
* <del>C# (430k)</del> (See Java)
* <del>Objective-C (281k)</del> (Irrelevant)
* Golang (109k)
    * Not my favorite, but probably the best compromise.
    * Excellent uptake.
    * Annoying that no generics, but whatever.
    * I don't worry about GC.
* <del>Swift (103k 300 jobs)</del> (Irrelevant)
* *These languages have >100k active repos*
* Scala (63k, 674 jobs)
    * Runs on JVM. Slow to compile.
    * Probably doesn't give great perf.
    * Prolly prefer Clojure.
* <del>Lua (46k, 64 jobs)</del> (Irrelevant)
* Haskell (45k, 29 jobs)
    * Hard to reason about perf.
    * I want control over memory. I want control of mutations.
    * Cf., Quicksort.
    * Generally a mindfuck.
* Clojure (36k, 89 jobs)
    * I worry about efficiency.
    * 2nd class citizen of JVM.
    * JVM startup sucks; programming Go is more fun.
* *This languages have >15k repos*
* Rust (15k, <10 jobs)
    * Blazing fast.
    * Very new. Super safe, but very static.
    * Not very high productivity.
    * **I think I like this best**
* Erlang (15k, 47 jobs)
    * Nobody says this is fast.
    * Not that fast.
    * Immutability is not my favorite.
    * Is distributed systems first.
    * Dynamic typing.
* <del>Common Lisp (10k, ~10 jobs)</del> (Poor uptake; and would prolly use Clojure instead)
* <del>Elixir (9k, 19 jobs)</del> (Poor uptake)
* <del>OCaml (7k, 10 jobs)</del> (Poor uptake; and would prolly use Haskell instead)
* <del>Scheme (7k, ~10-20 jobs)</del> (Poor uptake; and would prolly use Clojure instead)
* <del>D (7k)</del> (Poor uptake)
* <del>Julia (6k, <42 jobs)</del> (Poor uptake)
* <del>F# (6k, 18 jobs)</del> (Poor uptake)

## Basic Values

* Good uptake.
* Safety. No unforced errors, please.
* Parallellization.
* Low boilerplate.
* Good debugging, testing, profiling.
* High control. I need to be able to get low level.
    * Basically rules out immutability.
* Fast iteration/compilation. Fast startup.

## Summary

* For webdev, use Ruby or Python as iteration is much more important.
    * I have the highest productivity in these languages.
    * Node is an annoying ecosystem, and async everywhere is fucking
      annoying.
* For pure performance, use Rust.
    * Safer than Golang, because force you to think about ownership.
    * If I used inotify for recompilation, or they improve the
      compiler, that could be a gamechanger.
    * But still really new.
* Golang is probably a better compromise of great perf, but faster
  development.
    * Golang is going to trump Scala. No one says Scala is more fun
      than Golang, except Haskell people, so fuck that.
    * Clojure *would* be more fun to use, but the ecosystem is such a
      mess (prolly because it's not widely used).
    * Esp when you take into account making code fast, trying to
      optimize...
* Haskell is insanity (I tried **again**; and it didn't give me great
  perf). Erlang is just untested, but not promising, and not widely
  used.

## Go and Rust Thoughts

* In Go it is inconvenient to have thread-local global variables. In
  Rust this is simple.
* OTOH, is creating object pools simple in Rust? I did this to
  substantially speed up my Golang code, but I think it could be very
  difficult to implement in Rust.
* Maybe I should try Rust development with inotify. I feel like Rust
  is just more promising.

## Learning More Go!

* Go generate is a basic tool to generate code. Not super
  sophisticated.
* Web Dev:
    * gorm (3.5k) and gorp (2k) are the top choices.
    * pq (2k) and go-sqlite3 are the top DB drivers.
    * Most people use the stanadard templating builtin to Go.
    * Gorilla is popular for routing and websockets.
        * Seems like it does express basically.
        * httprouter is a more popular router alternative to
          gorilla/mux.
    * Martini (8.3k) is a Sinatra-like framework and the most
      popular. Provides basically nothing though.
    * Revel (6.5k) is the most popular alternative. Seems like losing
      ground to beego (6.7k). Gin is also popular (6.1k).
        * Beego feels very railsy.
    * godep
    * cli: for command line args
    * go-json-rest (2.2k)

## Learning More Rust

* Iron: Webframework
* Nickel.rs: Another webframework

There are basically no popular libraries.
