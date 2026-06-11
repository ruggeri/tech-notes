# Interpreted Languages Hosted on the JVM: Graal, Truffle, GraalVM

These notes are very high level.

Graal is an optimizing compiler, similar to the HotSpot C2 compiler.
(BTW, C2 is based on work by Clifford Click - get it?). Graal is newer,
written in Java instead of C++, and designed to be easier to extend and
experiment with. C2 is more battle-tested and optimized over more years,
but Graal is designed to allow extension (especially to support
Truffle), which would have been hard to add to C2. Graal and C2 largely
work the same way, performing the same kind of optimizations of Java
code, with broadly similar performance (most of the time).

Graal also supports **Native Image**, which compiles a Java program to
native code. This code must be a bit limited; it can't do reflection,
for instance. Native Image is one way to get faster startup. So Graal is
not *just* about Truffle, but Truffle is probably *the* killer feature.

The JVM has the ability to select C2 or Graal.

The key is *Truffle*. Truffle is a Java framework for writing an AST
interpreter in Java. You can run a Truffle interpreter with regular
HotSpot C2. But C2 cannot "see into" the AST to perform deep
optimization. For instance, if a node in the AST represents addition of
two ints, then we want to (1) avoid boxing the ints, and (2) perform a
simple CPU add instruction. We want devirtualization, inlining, and
optimization.

But usually HotSpot C2 can't figure that out. For one: the AST node is
not an instance of a class as specific as `AddTwoIntsNode`. The
implementation of `Node#eval` is likely megamorphic, dispatch can be
sophisticated, and not based merely on class.

Truffle can make sure that Graal is aware of the syntax tree structure,
so that Graal can optimize *into* the tree.

That is: Graal will JIT deeper into the interpretation of the
interpreted language.

GraalVM packages the two.

Major projects built on Graal/Truffle include:

- **TruffleRuby**: launched ~2013. Reaches production ~2019. Supports
  Ruby 3.4 as of 2026.
  - It seems like most people who want to run Rails reach for JRuby
    instead of TruffleRuby?
  - I don't totally understand why. JRuby has been around longer
    (started in 2001, reaches 1.0 in 2007), so maybe that's a reason.
    TruffleRuby seems to offer more C-extension compatibility hope?
- **GraalPy**: reached production ~2019. Supports Python 3.12 as of
  2026.
- GraalJS: reached production ~2019. Supports ECMAScript 2025 as of
  2026.. Used mainly for allowing JavaScript scripts/extensions that use
  Java functionality.
- FastR: production ~2019. Possibly dead.

Last: `InvokeDynamic` as another avenue to get better performance out of
dynamic languages hosted on the JVM. Look at `invoke-dynamic.md` for
more details.
