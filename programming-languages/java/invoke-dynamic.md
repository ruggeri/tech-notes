# Background: Importance of Inlining for Optimization

Java is intended to be portable, so limited optimization is performed
before runtime. Java code starts out being interpreted, and then hot
paths are JIT compiled/optimized at runtime. JIT can perform
"profile-driven" optimization, which in theory might result in *better*
optimization than static.

For optimization, inlining can be very important. Analysis/optimization
often does not occur across function boundaries unless inlining is
performed first. But inlining isn't free (it expands program size, can
make branch prediction/profile-driven optimization harder, often hurts
instruction cache). The compiler doesn't want to inline everything,
because inlining has cost. So the compiler only performs inlining when
it feels pretty sure it can expose optimization opportunity.

And, of course, you can't inline mutually recursive functions!

# Inlining and Virtual Method Calls

Inlining is further made more difficult when a virtual function is
called. In that case, the function being called is not even known until
the object's class is known. It may be in a totally separate translation
unit. So we cannot inline it before program start.

To see the limits of inlining, it's worth considering `qsort` from C, vs
C++'s templatized sorting. `qsort` gets a function pointer to compare
values. But there is *only one* copy of `qsort`. So we don't expect the
C compiler to inline the comparison function into the `qsort` method.
And that's assuming we enabled link-time optimization (LTO) to perform
optimization across translation units. The C compiler might inline
`qsort` to the call site, which could help. But if `qsort` is many lines
of code, it may look too costly to inline.

On the other hand, C++'s templatized sorting generates one function for
each comparator. That function is specialized to the comparator, and it
is natural to inline the comparison. This also introduces a strong
possibility for vectorization, BTW.

# Method Caches at Runtime

The JVM does *runtime program optimization*. That means that it can use
"profile-driven" optimization. For instance, even if *static* analysis
can't prove that a callsite is monomorphic, the runtime profile may show
it is. In that case, the JVM will often begin to optimize the callsite
(while leaving fallback code).

Elsewhere, I described *inline caches* and *polymorphic inline caches*.
These are conditionals that *devirtualize* a virtual function call.
Instead, you check the receiver class (not a vtable or itable), and if
it matches, you perform a standard function call. This potentially
avoids some virtual/interface method resolution overhead. But more
significantly, you can begin to inline the resolved function. That lets
you start to optimize.

Of course, you must fall back to the virtual method call if the object
class does not match. The HotSpot C2 compiler knows that this kind of
optimization *might* be useful whenever JVM bytecode uses
`InvokeVirtual` or `InvokeInterface`.

# Megamorphic Call Sites

The problem is that megamorphic call sites (where many different kinds
of objects are handled) typically don't get this kind of optimization.
The C2 compiler typically won't set up a PIC when the function is
megamorphic, because the code-bloat cost are too high. That means that
the invocation is not devirtualized, and optimization cannot be
performed.

Similarly, if the called function is very long and complicated, but
could be significantly pruned/optimized if only the receiver type were
known, that often won't happen, because HotSpot C2 would first need to
inline the function (which it often considers too speculative) before
pruning the branches.

What C2 can't do is "push" the caller's receiver class constraint down
into the called function to specialize it, because that called function
often *can't* be specialized for this particular call location. There's
only one copy of that function...

# Hosting a Dynamic Language on the JVM

Now, let's consider the problem of hosting a dynamic language on the
JVM. The approach we are considering is to compile the dynamic language
source code to JVM bytecode, then execute the bytecode. This is what
Clojure does for instance: it compiles clojure source code to JVM
bytecode in normal `.class` files.

Even though Clojure gets compiled to JVM bytecode, the bytecode can
still be mapped to Clojure source file names and line numbers.
Clojure-aware debuggers can still put breakpoints at appropriate JVM
bytecode positions. Stack traces can still use Clojure-like names (so
long as the clojure compile chooses class and method names that are
similar to the Clojure source code names).

# Dynamic Method Resolution

Now, sometimes the dynamic language compiler can know the type of a
method receiver statically. In that case, it can often emit JVM bytecode
to perform a static function invocation. But often the type is *not*
known or inferrable at compile time; that's the point of a dynamic
language, after all. In that case, dispatch must be performed
dynamically.

When the receiver type is practically unconstrained, method dispatch is
often fairly complicated, and difficult for the C2 compiler to optimize.
You can imagine doing dynamic dispatch via reflection, which is
basically looking up strings in a table per receiver class. Keep in
mind: the interpreted language may have multiple inheritance (while JVM
bytecode has single-inheritance virtual dispatch), or even
monkey-patching (method replacement) at runtime. So the dynamic method
resolution model may not even map well onto the JVM model (which is
designed for Java).

The problem is not only the dynamic method resolution overhead. The
problem is also that any `resolveDynamicMethod(receiver, methodName)`
implementation is going to be *megamorphic* and not particularly
inlineable.

And this is a problem, because a lot of call sites are monomorphic.
Which means, we want to use an inline cache to turn the dynamic method
resolution into a static method call (with fallback). And we want to
inline the static method call for optimization.

And this problem is almost everywhere, because almost always the final
type of the receiver is not known, and the resolution rules even of
*subclasses* can be very complicated. That's the *point* of the dynamic
language.

# Can We Implement Our Own Inline Cache?

You could try to setup your own method cache. You can store the receiver
class for comparison, and the method that needs to be invoked.
Subsequent calls can avoid dispatch and invoke the method. Something
like:

```java
class DynSite {
    Class<?> cachedClass;
    MethodHandle cachedTarget;

    Object call(Object recv, Object arg) throws Throwable {
        if (recv.getClass() == cachedClass) {
            return cachedTarget.invokeExact(recv, arg);
        }
        return fallback(recv, arg);
    }
}
```

This still won't really work. You'll need a `DynSite` class for *each*
call site! Otherwise, `DynSite#call` is megamorphic, and won't get
optimized. Even if you have one class per call site (which sounds like a
lot of bloat!), you'll still need C2 to learn that `recv`, `cachedClass`
and `cachedTarget` are all stable enough that it is worth inlining the
call to `cachedTarget`.

And all your `DynSite111`, `DynSite222` classes are kind of a clutter to
the debugger, and are bloat.

# `invokedynamic`

`invokedynamic` is basically the solution you want. This instruction
calls into a bootstrap method. The bootstrap method can setup an inline
cache, just like `DynSite`. Except it does so in a way that HotSpot is
more aware of: it uses classes like `CallSite` and `MethodHandles`.

Basically, everything we are doing in the bootstrap method is basically
the kind of stuff we did in `DynSite`. But using stuff like `CallSite`
and `MethodHandles` is blessed by Java. We can expect Java to better
understand that this is call-site linkage code, not arbitrary Java code.
HotSpot is designed to specifically optimize this restricted, call-site
linkage specific subset of Java.

# Clojure Doesn't Really Use `invokedynamic`

Clojure function invocation is largely *virtual*. Functions implement
`IFn`. They don't go through some crazy dispatch function. So HotSpot
knows how to optimize that. So we don't need InvokeVirtual for this.

Clojure multimethods/`defmulti` are very flexible, but dispatch is *too*
dynamic to expect to be translated to static dispatch and inlined and
optimized.

There are also protocols/`defprotocol`. This is more like interfaces,
and uses `InvokeInterface`. But protocols can also extend existing
classes and interfaces. Because dispatch in that case might be class
based, that does suggest that you could use ``invokedynamic``.

But I think Clojure doesn't bother? They figure that most protocols are
defined on types at definition time (so you can do regular interface
dispatch). They treat extension of existing types as an edge case not
worth optimizing. Clojure predates ``invokedynamic``, but that can't be
the whole story. Else it seems lazy not to use that feature; it's not
like they can't update the compiler.

One thought: Clojure allows redefinition of the vars. The easiest way to
handle that is to bust all caches and deoptimize everything.

At the end of the day, it sounds like `invokedynamic` doesn't offer
Clojure *a lot*, because a lot of dispatch is either static or virtual
already. Or is too dynamic for the compiler to know how to optimize for.
The best case is relatively narrow: protocols extending existing types.

But Clojure treats that as too niche to use `invokedynamic` for. Honestly,
I think they could/should do this. But I think Clojure feels: you
shouldn't be doing this kind of protocol dispatch in performance
sensitive code.

# Users: Groovy, JRuby, and Java itself

Besides Clojure, Groovy and JRuby are the two most common dynamic
languages that compile to JVM bytecode as of 2026. Graalpy and GraalJS
don't need `invokedynamic`. Graal is an orthogonal direction where you
*interpret* the hosted language, but where Graal will optimize *into*
the AST.

And Java itself uses `invokedynamic` for anonymous function *creation*.
Java needs to create an (anonymous) instance of `Function<InputType,
OutputType>`. When the function is used, the interface method
`OutputType apply(InputType)` will be invoked (by `invokeinterface`).

One way Java could implement the anonymous function is generation of an
*anonymous inner classes*. But this creates additional class files.

But what Java *actually* does is compile the anonymous function/lambda
to a regular function. At the callsite, we use `invokedynamic`. This
calls `LambdaMetafactory`, asking it to instantiate an instance of
`Function` *at runtime*. The `#invoke` method just calls the previously
compiled method.

In fact, `invokedynamic` allows the strategy for invoking lambdas to
*change* in the future versions of Java.

You also gain the possibility that stateless lambdas (which don't
capture variables) might be cached by the meta factory (instead of
repeatedly instantiating inner class instances?).
