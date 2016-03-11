# Joy of Clojure Notes

## TODO3

* Serialization?
* Async: no fibers, don't block on IO!
* Korma seems basically unmaintained
* Still have no idea about multimethods...

## Basics

* destructuring rules
* symbols: resolved relative to current namespace unless qualified.
    * binding
* `recur` for tail recursion (can use `trampoline` too!)

## Iteration

* `loop`/`recur`
    * call to `recur` will jump back to the loop.
    * `recur` needs to take new values for the loop call.
* `for`
    * Builds a lazy-seq: `(for [x (range 100) y (range 100)] (+ x
      y))`.
    * Each time around the vars get set to the next value.
    * Iterates through the rightmost fastest.
* `doseq`: like `for`, but doesn't return anything, just executes
  **everything**.
* `dorun`
    * Forces evaluation of seq; does not retain head, returns nil.
* `doall`
    * Forces evaluation of seq; retains head, and returns it.

## Macros

* `'xyz` means `(quote xyz)`
* The syntax quote (backtick) recursively quotes everything within.
* To stop the recursion, use `~` (unquote) which will inject the
  value.
* Or, use `@~`, which will inject a sequence of values.
* The syntax quote will also try to fully qualify symbol names.

```
(defmacro x [] '(+ 1 2))
(macroexpand '(x))
=> (+ 1 2)

(defmacro sum [& args] `(+ ~@args))
(macroexpand '(sum 1 2 3))
=> (clojure.core/+ 1 2 3)

(defmacro global-sum [] `(+ a b))
(macroexpand '(sum))
=> (clojure.core/+ user/+ user/+)
```

* You can use `~'local` if you need an unqualified variable name
  inside a syntax quote.

## Collection Types

* All these structures are "persistent" of course.

* `seq`/`first`/`rest`/`next`
    * `next` calls `seq` on the `rest` result.
* `cons` takes a seq and creates a `Cons` obj; it's for heterogenously
  prefixing on an element.

* You can use `vector-of` to make a vector of primitive objects. This
  should be more mem efficient, I guess.
* Vectors: `assoc`/`get`, `conj`/`pop`/`peek`
* Slow to remove except from end, but subvectors are fast.
* Vectors make poor queues (subvec maintains reference to original
  array); use `PersistentQueue` in this case.
* Can index like so: `(v 123)` gets 123rd el

* Lists rarely used over vectors.
* `conj`/`peek`/`pop` work like before
* Seqs aren't lists of course... For instance, PersistentList has O(1)
  count.

* PersistentQueue
* Implemented as a front seq and a back array. When front is done
  being iterated through, back array is seq'd and a new back array is
  created.
* As ever, `conj`/`peek`/`pop` are your friends.

* PersistentSet
* Acts as a function; returns matched el or nil: `(#{:a :b :c}
  :c)` returns `:c`.
* Uses `=` to determine duplicates.
* `contains?` works on set; doesn't exist for `list` and works for
  indices on a vector.

* PersistentHashMap/PersistentArrayMap
* keywords are functions that look themselves up in a map
    * this also works for sets :-)
* Also a sorted version; uses either a `<` true/false returning
  function, or a `compare`-style function returning -1, 0, 1.

## LazySeq and `delay`/`force`

* Use the LazySeq macro
* It will return a `LazySeq` object which will only invoke the body
  when `seq` is called.
* Thus you write things like:

```
; this also uses that (cons 1 nil) is '(1)
(defn range [start end]
  (lazy-seq
    (cond
      (>= start end) nil
      :else (cons start (range (inc start) end)))))
```

* Be careful not to keep reference to the head of the lazy seq as you
  iterate through, otherwise you could run out of mem.

```
(def my-map [f coll]
  (lazy-seq
    (if (seq coll)
      (cons (f (first coll)) (my-map f (rest coll)))
      nil)))
```

* Note that if we used `next` instead of `rest` here, we'd force
  evaluation of one element more than is necessary.
* There is also a `defer` method; it is a macro that creates a
  thunk. You can `force` the evaluation later.
* Good exercise: implement a lazy quicksort

### Write your own seq

* Implement the `ISeqable` interface. But you need a real java class
  to do this.
* Then you're going to need to define an `ISeq` type...
* Easiest way is prolly just to make it `Iterable`, in which case
  Clojure does this for you.

## OO

### Multimethods and records

* `defrecord`/`defmulti`/`defmethod` are great.
* Uses `isa?` for comparison. You probably want to use class as the
  dispatch function normally.
* You can use `derive` for ad-hoc hierarchies.
* You can use `prefer` to prefer the subclass's methods over the
  parent's.
* **TODO3**: is this correct? To use prefer, it seems to me you need to
  know which superclass actually implemented the method. What if you
  prefered against the grand-parent, but then add a parent
  implementation?
* **TODO3**: I have worries that inheritance in this world could be a
  bear.
* **TODO3**: Fogus recommends UDP (basically prototypal inheritance) as
  a design pattern. Maybe that's not so bad an idea? Is there a
  "compose" type macro (as in Go)? Could I write one easily?

### defrecord

* Ugh. BTWs, you need to import defrecord classes explicitly (with the
  java import syntax).
* **TODO3**: Book claims you can't deserialize a printed record; that's
  bullshit. I'm pretty sure this has improved since publishing.
* Interesting: you can assoc/dissoc any key you want; but unexpected
  keys have map style performance.
    * If you dissoc a required key, you'll get a map. Whatever, this
      is bullshit.
* BTW: you can override object methods in a defrecord.

### defprotocol, extend

* In lieu of `defmulti` and such, you can define a protocol
  `defprotocol` and use `extend-type` on a Java class.
    * This does class-based dispatch.
    * **TODO3**: It's supposed to be faster?
    * Works great for `defrecord`s, obviously.
    * `extend` is the original; `extend-type` is just mildly more
      convenient.
    * `extend-protocol` allows you to define several implementations
      for different classes in a row.
* Clojure doesn't give you an easy way to extend your record class.
* Instead, you can break out the implementation of a protocol to an
  object of methods. Then you can use this in "subclasses".

* `reify` is a way to make an instance of an anonymous class that
  implements a protocol.
* This allows the method implementations to close-over outside
  variables, but why shouldn't that be a member?

```
(defn fixed-fixo
  ([limit vector]
    (reify FIXO
      (fixo-push [this value]
        (if (< (count vector) limit)
          (fixed-fixo limit (conj vector value))
          this))
      (fixo-peek [_]
        (peek vector))
      (fixo-pop [_]
        (pop vector)))))
```

* Finally, `deftype` is like `defrecord` but without any of the usual,
  built-in methods.
* **TODO3**: how is all this implemented?

## var/ref/atom/agent

### var

* `def` creates a `var`, a thread-local mapping; that symbol will now
  refer to a value.
    * If you mark the var `dynamic`, you can use `binding` to later
      rebind it. We normally write this like `*name*`.
* `set!` can re-bind a var, but it won't affect other threads.

### ref

* Use `ref` if you want to share data across threads.
* `dosync` starts a transaction; inside a transaction you can
  `ref-set` or `deref`. You can use the `@` shorthand to deref.
* You can use `alter` as a convenience if you want to apply a function
  to calculate new value.
* `commute` allows greater concurrency; you give it a func, and it
  applies it at the end of the transaction.
    * Bizarely, commute runs and returns a value inside the
      transaction
    * I would have expected it just to wait until the transaction end
      to run on the current value.
* TODO3: When to use `ensure`? Probably about write skew or something?
* Of course, since the transaction may be re-run, keep it pure in
  there.

* TODO3: Livelock can happen (barging tries to solve this??)
* TODO3: You can use `min-history`/`max-history` on refs to tune STM
  performance. Look this up??

### atom

* Use `atom` as a simplification of `ref`.
* Again have `deref`/`@`.
* Also you get `swap!`. This will set the value within a transaction.
* They spin-loop rather than use the STM. For this reason, they can't
  be rolled back, so don't use them in a transaction.

### agent

* Use `agent` for sending functions to agents, have them
  asynchronously do the work.
* A threadpool switches amongs the many agents, doing work on each of
  them.
* Again with the `defer`/@
* Use `send` to send work; it uses a bounded thread pool for
  efficiency.
* Be careful; if the work can block, you want to use `send-off` to
  avoid using the whole pool.
* That's ugly. I wonder if core.async works nicer...
* TODO3: You have to handle exceptions for the agent, but I'm going to
  ignore that for right now.

## Concurrency continued

* STM plays nice with immutable data structures, but not with mutable
  ones.
* What do you do when you need to modify, transactionally, two
  **arrays**.
* There's a locking macro which will lock an object.

* Give `future` a function and it will run it on another thread for
  you. You can then use `@` to block and wait for the result.
* There's also a `promise`; this creates an object that you can deref
  (blocks), and which waits until someone calls `deliver` on it.
* `future` is more orriented toward asking another thread to do the
  background work, whereas `promise` could be satisfied by any thread.
* http://stackoverflow.com/questions/4623536/how-do-clojure-futures-and-promises-differ

* `pvalues` creates a seq where values are calculated in parallel. It
  is semi-lazy: all the values in a range (sliding window) are
  calculate simultaneously. Each time you read one, it starts working
  on the next in the background.
* Similarly for `pmap`.
* Lastly, there's a `pcalls`, which takes a sequence of functions,
  calls them in parallel, allowing you to lazyily iterate over
  results.

## namespaces

* Use `ns` to start a new namespace.
* Privacy is set on metadata; you can use `defn-` as shortcut.
* `(ns xyz (:use [clojure.core :only [seq]]))`
* `:require` pulls in the library, but you need to use the qualified
  namespace.
* You can `:use` wiht `:as`
* `:import` is for java classes

## Java Classes

* There's a `proxy` method that works like `reify` but creates an
  anonymous subclass of a base class.
* There's also a `gen-class` method to generate a named Java class. I
  skimmed and mostly ignored this part.

## Performance

* Type-hints can avoid reflection, thus increasing speed.
* You can hint either at the method, or just in-line with a value.
* **TODO3**: I guess I thought that JIT would help here?
* Transients are good for producing a mutable collection from an
  immutable one. They are faster because they require less allocation.
* Clojure uses a granularity of 32 in chunking sequences.
* There are also primitive conversions for faster math, but recal that
  only objects are passed in/out of functions.

## Keyword Vs Symbol

* Keyword is an identifier with fast equality. It is frequently used,
  e.g., as a key in a map.
* Symbols are also identifiers. They are typically used as names for
  other things.

Why have both? I think the main reason is that, when evaluating an
s-expression, a keyword is evaluated as itself, whereas a symbol is
replaced with the value it refers to. This is important for
metaprogramming via macros, I believe.
