# Promises, Generators, Fibers

## Promises

The point of **promises** is to avoid the pyramid of doom and passing
callbacks everywhere. Rather than have every method that does some
async work take a callback to complete the work, you can instead call
`.then` and pass a callback. This callback can then itself return
another promise, so that you can call `.then` again.

This avoids a "pyramid of doom", since you chain `then`s in a
sequence, not deper and deeper. However, you acn already avoid this by
using named functions.

Another advantage is that not every function that happens to call some
async function needs to take callback arguments; instead, we just
return the promise.

These aren't really game-changers though. Especially when you consider
that using promises seems t me to make it potentially harder to do
cool concurrency patterns as with the `async` library.

## Generators and Coroutines

Generators are like functions that can return a value, but when
invoked again restart from where they last returned. Languages that
support generators effectively give syntactic sugar for an
"enumerator" class.

Generators are sometimes called **semicoroutines**. When they return,
they must return to their caller, like a normal function. This is
different than full **coroutines**, which allow the coroutine to
resume any other coroutine.

You can implement coroutines from semicoroutines with a trampoline in
`generator.js`.

**Fibers** are really the same idea as coroutines. Whereas coroutines
are a language construct, fibers are the same idea taken to the OS
level; they are an alternative to threads. Instead of being
pre-emptively switched, they explicitly delegate to another
fiber. This is kind of like how threads are the OS level version of
green threads. Ruby kind of confuses this by using the word `Fiber`
for coroutine.

Using coroutines (or generator + a trampoline), you can do async code
in a straight-line way. This is effectively what the promise +
generator + spawn thing was that Ashi showed us.

## Continuations

JavaScript adds generators as sort of a "pseudo-function" that
effectively builds an iterator class. Ruby likewise has a bogus
`Fiber` class to implement coroutines.

Scheme's `call/cc` is arguably more elegant, since it can be used
anywhere to generate a function with the *current continuation*.

Without `call/cc`, but with closures and a trampoline, you can acheive
the work of `call/cc` by writing in *continuation-passing style*.

## Trampolines

Note that trampolines can be used to implement tail-recursion.
