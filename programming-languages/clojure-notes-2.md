* `deps.edn` is easiest way to declare dependencies.
* def and defn
* fn or #(+ %1 %2) to define anonymous function
* let bindings
* Closures work like expected.
* You can instantiate classes like `(Widget. "input")` or methods like
  `(.method object)`.
* Vectors have syntax of `[1 2 3]`. Conj and pop add and remove. For
  sets `#{1 2 3}` we use disj.
* `assoc` and `dissoc` for maps. `contains?` for maps and sets.
* Maps can be used like objects. `(my-map my-key)` works like `(get
  my-map my-key)`. Because `my-map` must implement the right
  `IFunction` interface or whatever.
* `defrecord` defines a record, which is much like a hash map, but
  with fixed fields and better performance.
    * Note that it is recommended that you write your own simple
      constructor functions.
    * For instance, write `(kitten { :name "Gizmo", :age 10 })` rather
      than simply `(Kitten. "Gizmo" 10)`. This works well if you write
      `(defn kitten [{:keys [name age]}] ...)`.
    * defrecord is preferred to deftype
* `do` lets you have a series of statements. It's useful when you have
  side effects.
* `if` takes two branches, `when` takes one. `cond` takes a series of
  expressions and does the first one.
* They have `dotimes` and `doseq`. Both of them are eager; `doseq`
  forces evaluation and does something each time.
  * `(doseq [x (range 3)] (println x))`
  * Notice the bindings.
  * You can do the same kind of binding, but lazy, with `for`.
  * `dorun` evaluates a lazy sequence and throws it away.
* `loop` and `recur`. `defn` creates an implicit loop binding. `recur`
  must be in tail position.
* `try`/`catch`/`finally` work mostly like always.
* `require` pulls in libraries: `(require '[clojure.string])`.
  Presumably quoted so you don't have to use quotes yourself.
* You use `(ns myproject.welcome (:require [myproject.person-names :as
  pnames]))` to:
    * Start a new namespace for your file.
    * You'll be able to use, in this namespace, anything from
      `myproject.person-names` as `pnames`.
    * Typical is that this file is in `src/myproject/welcome.clj`.
* There is `->` and `->>` for threading through a series of expressions.
* You can interact with Java via `(. object method)` or even `(.. object
  (method1) (method2))` which does threading.
    * There is a concept of "proxy" which allows you to implement a Java
      interface.
* `atom`s are like `ref` but never coordinated with other arbitrary
  `ref`s. They're like `agent` in that they manage a single value, but
  they are synchronous. They basically do test-and-set.

**Mutable Places**

* `(def x)` creates a `Var` called `x`. It can be rebound to a new value
  on a per thread basis.
    * there is a `^:dynamic` metadata option to allow "rebinding", which
      means you can say `(binding [x 1 y 3] my-fn)`, and this will set
      `x=1, y=3`, and calls `my-fn` where those values will be used. The
      binding is temporary.
* You can do cross-thread with STM with `ref`.
* Agents let you send functions that update their state, maybe sending
  more messages. The messages are not sent until the agent's state has
  been updated. You can use `send-off` if there is blocking IO (will
  fork a thread). Else `send` uses a thread pool.
    * An HN thread said that agents are hardly used by anyone.

## Concurrency

* `var` is a thread-local storage; you can change it, but changes are
  only visible inside the same thread.
* A `ref` is like a `var`, except you can use it for STM. That means
  that inside a transaction you can use `ref-set`.
    * The HN people say that STM is hardly used.
* `atom` is like `ref`, but simpler, for uncoordinated changes. It
  just does a test-and-set thing.
* `agent`s are Clojure's analogue to actors. You can send an agent a
  *function*, which is asynchronously applied. You can deref
  synchronously.
    * Why not actors? Well, Hickey says that actors means you have to
      send a two messages just to read the state of an actor (one
      forward, one back). He says that's unnecessary when in the same
      process (actors are better suited for distribution).
    * Likewise, I think he suggests that actors are not particularly
      flexible. In that, they aren't "open." You can only send them
      messages that they are designed to receive.
* `core.async`:
    * Here you have channels and "goblocks."
    * Basically, in a `go` block, channel operations that would
      normally block a thread just suspend that goblock.
    * `alts!` is there version of select.

## Typing

You can do a type-annotation thing using a library called
`core.typed`. Here is a good example:

    https://github.com/clojure/core.typed/blob/master/module-check/src/test/clojure/clojure/core/typed/test/rps_async.clj

That library looks almost like a potential game changer!

**TODO**

* Maybe easiest to use deps.edn and simple setup for Clojure.

https://clojure.org/reference/multimethods
https://clojure.org/reference/protocols
