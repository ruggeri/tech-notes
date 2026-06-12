# C++ Multiple Inheritance

## Multiple Inheritance of Fields

A single inheritance of a struct just extends it with more
fields. Multiple inheritance will have the first base class (B1)
fields first, followed by B2's. You can cast to a pointer of B2, which
should "fixup" the pointer by skipping the B1 fields. Any new fields
come after B1 and B2.

This way casting to a B1 or a B2 has the expected layout.

## Multiple Inheritance of Methods

In single inheritance, there is a VTable. Every instance has a pointer
to the VTable. The VTable of a childclass has the same layout as its
parent, except when you override a method you replace it in the
VTable. Thus code that takes a pointer to B1 can call the override
method in the child class. Also, new methods in the child class can be
added to the end of the VTable.

In the case of multiple inheritance, you use additional VTables. First
comes the VTable from B1, followed by fields of B1, followed by the B2
VTable and fields. Now you can safely treat a pointer to a child
instance as a B1 (with the usual VTable format), and the cast to a B2;
the fixup skips B1's methods and fields, and uses the usual B2 VTable
layout.

New methods in the child class can be added to either the B1 or B2
VTable, it doesn't matter which particularly. Or they could even be
added in a third VTable at the end of the B2 fields.

Note that a VTable is just a pointer; otherwise you'd bloat every
object by an amount equal to the number of virtual methods! But that
costs another indirection...

## Casting and Pointer Fixup

Because of the fixup, if you cast from `ChildClass` to `B2` (via a
`static-cast`), and then do a `reinterpret_cast` to `B1`, this will be
an error, because you'll have the `B2` layout!

To support `dynamic_cast` allowing safe downcasting, objects need RTTI
to check the cast. This could be stored in the VTable. In the case of
multiple inheritance, the `B2` VTable also needs it; in fact, it needs
to record how much to un-fixup the pointer!

## Diamond Inheritance

When both parent classes share a method, you need to use explicit
qualification to name which one you want to use: `child->Base1::f()`
vs `child->Base2::f()`. Otherwise it is of course ambiguous. You can
resolve this by overriding `f` in `Child`. Same thing with casting.

## Virtual Methods and Virtual Inheritance

It is common for a class to satisfy an *interface*. A pure interface
class may have just virtual methods. The implementation class derives
the interface class and implements the methods. Now it can be passed to
functions that expect the interface to be satisfied.

Another idea is a *mixin* class. Here you have some virtual methods, but
then you have other methods implements (and possibly even fields). A
class can extend the mixin; the intention is that the subclass will
implement the virtual methods.

You might separate an `Iterable` pure interface class from an
`IterableMixin` mixin class. In that case, a base class might extend
both `Iterable` and `IterableMixin`. Since there really should only be
one copy of the `Iterable` methods, you should extend both `virtual`-ly.
This will allow you to pass the object anywhere an `Iterable` is
required.

It also helps if you wanted to add a second mixin which extends
`Iterable`. If you had `Derived : IterableMixin, IterableMixinTwo`, then
`Iterable& i = d` is ambiguous (and gives an error) unless you derive
virtual.

With that said, I think you only truly *need* to do virtual inheritance
when sister classes want to call each-other's implementations. I show an
example where I do this in `Collatz` in `diamond-inheritance.cc`.
Frankly, that's an odd pattern. I believe that this is called
"sister-class delegation".

## Interfaces in C++

Note that, as in the preceding example, you can inherit from a pure
virtual class. This causes a VTable to be added. Now you can pass the
object to anyone who takes a pointer to the base class type. If you
implement multiple interfaces, you just have multiple VTables.

# Interface Dispatch And Java

In C++, you can implement multiple interfaces by deriving multiple pure
abstract base classes. These classes could even define some concrete
methods for some default behavior.

Java does something slightly differently. Firstly, it separates the
concept of `interface` and `class`. An interface cannot have fields.
They used to allow *no* concrete methods, but Java 8 (2014) added
default method implementations.

You might imagine that Java does the same thing as C++: have subsequent
vtables cooked into the object. Instead, the object header points to a
list of itables. When you pass the object somewhere which expects the
interface, you still just pass the object pointer. There is no fixup
like C++ does. Now, when an interface method is invoked, Java cannot
call it like a regular virtual method. Instead, it must use the
`invokeinterface` JVM bytecode.

This iterates a list of *itables*, stored in the class object referenced
in the object header. It finds the appropriate itable, and jumps to the
appropriate slot. It executes the method.

Why does Java do it this way? I think the answer is that (1) it doesn't
want the full power of multiple inheritance, and (2) it sees the object
pointer fixup as a bit of a mess. Possibly also it makes GC tracing a
bit more annoying if you have fixed up pointers.

Another contrast: C++ objects with a lot of pure abstract base classes
are a bit bloated from vtable pointers, whereas Java objects
implementing lots of interfaces have just a single pointer to a list of
itables which is shared for all instances of the class. OTOH, Java needs
to iterate the itable list to find the appropriate itable, so interface
method invocation might be a bit slower than C++ virtual method
invocation, which just jumps to the appropriate vtable. Though Java JIT
probably quickly learns to optimize away the itable scan.

Let's return to Java interface default method definitions. Obviously, a
pointer to the function is stored in the itable. But it is also stored
in the object vtable. This allows the method to be called normally with
`invokevirtual`.

Indeed, any time you call a method on a pointer to a class (not an
interface), you use `invokevirtual` and consult the vtable.

## Dynamic Dispatch: An Aside

In Smalltalk (and message-oriented languages), messages are sent to
objects. Before run-time, there is no information about what kind of
thing this is. The same message can be sent to two unrelated objects,
which can respond entirely differently.

How an object responds to a message can be very slow. You might iterate
through a list of the class's instance methods, doing string
comparisons. BTW, you probably want to intern strings to speed that
comparison up to just a pointer check. At a certain point, a hash could
be worth it for O(1) lookup. Probably if you use a hash table you'll
want open-addressing to avoid extra levels of indirection and cache
miss.

But you have a further problem with hierarchies. If you traverse a deep
hierarchy, this is slow. So you can cache, either at the per-class
level, or more simply globally (the key is `(typeid, message_name)`).
The global hash is simpler because it's easier to bust, which is
necessary whenever methods change.

Are hierarchies that deep? If they were flat, the global hash doesn't
buy us anything. It maybe saves an indirection to the class's specific
method hash, but that hash is smaller. It's considered a _refinement_
to have per-class caches...

Everyone says that the problem is with hierarchy depth. They say that
this can be particularly bad with Ruby modules.

## Inline Method Caching

One solution is to use *inline method caching*. At a callsite, the type of
the last object, and the address of the called method, is stored. If the
site is monomorphic, this can greatly speed up method invocation.

In case you have a polymorphic call site, you can replace with a switch
of a few type options, jumping to the appropriate method. This is
sometimes called a *PIC* or *Polymorphic Inline Cache*.

## Implementing Interface Methods

In addition to a vtable, also link to an array of _itables_, which can
be iterated. Check for the appropriate interface id, and eventually you
find the right itable. Now you have an itable with a fixed layout, so
you lookup at the offset and find the place in the vtable to jump to.

This is O(n) in the number of interfaces, and involves one more
indirection than the vtable.

There are some improvements that people have explored, but since
inline method caching still works, these don't seem vitally important.

I think it's common to move the itable to the front, as an optimization
to help with future calls to the same interface.

As usual, you can use inline method caching to devirtualize the method
call.

# Interfaces In Golang and Rust: Fat Pointers

Interface values always have a pointer to the data, and a pointer to the
itable. These are called "fat pointers" sometimes.

This avoids object bloat that would come from many vtables. It makes
interfaces cheaper than in C++. It also means that casting to an
interface doesn't require pointer fixup.

But the major benefit is ergonomic: post-hoc implementation of
interfaces. This is a major feature of Golang. You can define a new
interface, implement the methods of the object (which are just
functions, since there is no inheritance and no virtual methods and no
vtables). Now, when you pass the object to a method (or store it in a
variable) which expects the interface, that's when the wrapping happens.

Neither C++ nor Java allow this. All interfaces must be implemented at
class definition time. The best you can do is extend the class to
implement new interfaces.

## Rust Traits

Rust is similar. Like Golang, it has no inheritance. Unlike Go, it has
nominative (not structural) typing. That is: you don't implement the
interface just because you have the methods; you must tell the compiler
you want to implement the trait. Still, when you pass the object
somewhere that expects the trait, you convert to a fat pointer.

```rust
trait Draw {
    fn draw(&self);
}

struct Button;
struct Slider;

impl Draw for Button {
    fn draw(&self) {
        println!("button");
    }
}

impl Draw for Slider {
    fn draw(&self) {
        println!("slider");
    }
}

// Here `Draw` is used as a trait bound. This function will be
// monomorphized, there is no dynamic dispatch that will happen here. A
// "fat" pointer is not passed.
//
// We could even have passed `x: T` if we wanted, because the type will
// be known.
fn render<T: Draw>(x: &T) {
    x.draw();
}

// A shorthand for the template code with the trait bound.
fn render(x: &impl Draw) {
    x.draw();
}

// This is *not* a templated function. Instead, it needs a fat pointer
// and will do dynamic dispatch.
fn render(x: &dyn Draw) {
    x.draw(); // vtable call
}
```

An oddity: you can implement two traits for the same type, each of which
names the same method. It is not an error if they have different
signatures. Even if they have the same signature, they can have
*different implementations*.

One downside is that you will have to resolve the ambiguity in order to
call the method.

```rust
trait A {
    fn f(&self) -> String;
}

trait B {
    fn f(&self) -> String;
}

struct User;

impl User {
    fn f_impl(&self) -> String {
        "shared implementation".to_string()
    }
}

impl A for User {
    fn f(&self) -> String {
        self.f_impl()
    }
}

impl B for User {
    fn f(&self) -> String {
        self.f_impl()
    }
}

User u{};

// Can't just write `u.f`, since it's ambiguous what method to call.
A::f(&u);
B::f(&u);
```

## Rust: Aside about `impl Trait` return type

Basically, you can use `impl Trait` as a return type:

```rust
fn make_widget() -> impl Draw {
    // all return paths must return same concrete type.
    Button
}
```

You would do this if you are writing a library and you don't want to
leak details about the implementation of the widget returned by
`make_widget`. Your only promise to the user is that the returned object
implements `Draw`.

Basically, the *compiler* is going to know the type returned in
`Button`, but it won't let anyone else *use* that fact.

```rust
// The type of `x` is left "anonymous" and inferred as `impl Draw`. You
// only know it is a type that implements `Draw`.
let x = make_widget();

// Compiler can statically dispatch to Button::draw
x.draw();

fn runDraw<T: Draw>(obj: &T) {
  obj.draw()
}

// Compiler can still monomorphize.
runDraw(x);
```

The alternative would have been:

```rust
fn make_widget() -> Box<dyn Draw> {
    Box::new(Button{})
}
```

This is more like what C++ would do. But the cost is that you must
invoke methods virtually, and you lose monomorphization sites which can
be specialized and optimized.

**TODO**: I got carried away with notes on Rust. I should move these
Rust-specific notes out somewhere else...

# Sources

- Source: http://research.swtch.com/interfaces
  - Russ Cox: Go Data Structures: Interfaces
- Source: http://blog.rust-lang.org/2015/05/11/traits.html
  - Abstraction without overhead: traits in Rust Aaron Turon
- Source: https://news.ycombinator.com/item?id=9526440
  - https://news.ycombinator.com/item?id=9526440 discussion

[Efficient Implementation of Java Interfaces: Invokeinterface Considered Harmless][1]
[InterfaceCalls][2]

[2]: https://wiki.openjdk.java.net/display/HotSpot/InterfaceCalls
[1]: http://yanniss.github.io/521-10/oopsla01.pdf

- Another good source: https://lukasatkinson.de/2018/interface-dispatch/
  - I found this much later; it confirmed much of what I wrote here.
