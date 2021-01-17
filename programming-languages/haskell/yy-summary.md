# Summary of Types Encountered

**TODO**: Review me!

* Reducable/combineable things:
  * `Semigroup`: has an associative binary operation.
  * `Monoid`: a semigroup with an identity element. Examples are `Sum`,
    `Product`, `[]`, `Maybe`.
* Things that can interact with functions:
  * `Functor`: anything that is a box of a value where a function can be
    lifted to the box-level and applied.
  * `Applicative`: where a box of functions can be applied to an instance of
    the box.
      * TODO: not really sure of any great examples of something that
        feels very applicative to me?
      * `[]` is a natural example of `Applicative`.
  * `Foldable`: anything that can iterate its items. Anything with a
    `toList` method. Anything with a `foldr` method.
      * `fold` and `foldMap` are simple methods that can work when the
        `Foldable` contains `Monoid` values, or are mapped to some
        `Monoid`.
* Monads
  * `Maybe`: context is simply 'have I failed?' `>>=` threads the
    current value to the next function, but will ignore failed values.
    The function can succeed or fail.
    * In `do` notation, as soon as `Nothing` appears, then there is no
      recovery. `Nothing` must be returned. You could call this a
      'side-effect,' in that the use of the `Nothing` monad changes the
      context.
    * `Either` is a simple extension.
    * Basically, the monadic context (failure or success) affects the
      computation performed by the next function, in that the function
      is only run if the previous value was successful. Else the error
      is propagated.
  * `Writer`: keeps track of a `Monoid` state. `>>=` threads the
    current value to the next function. The function returns both the
    next value and anything to `mappend` into the monad context.
      * There is no way for the monad to read the accumulated context.
      * `Int`, `""`, and `[]` are all good choices for a context.
      * In `do` notation, you might encounter a `Writer` value that
        simply writes a log-line. It might *look* like it does nothing,
        but it a monadic action in the sequence described by `do`.
      * The context 'affects' the computation only at cash-out at the
        end where the accumulated context can be read.
  * `State`: broadens the allowed interaction with the state. `>>=`
    threads both the current value *and* the current state.
      * In this way, it is strictly more general than `Writer`.
      * The underlying state can always be promoted/got with the `get`
        `State`. But I suppose the power of the `State` monad is that
        mostly your actions will interact with the state behind the
        scenes, separating these underlying state interactions from your
        higher level code that just works with the values being returned
        by the state interactions.
  * `[]` monad
    * TODO: I would like to understand this monad better. It feels
      weird.
    * I've seen it said that this is like a non-deterministic
      computation. In our knights example, the search space exploded.
    * But how does this relate to the other monads we've studied?
