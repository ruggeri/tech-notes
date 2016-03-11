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

## Casting

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

Multiple inheritance is kind of uninteresting if two modules can't
"talk" to each other. The way to do this is to add a shared,
**virtually inherited** base class. This "delegation" class is pure
abstract, containing only virtual methods. Module1 can implement `f1`,
while Module2 can implement `f2`, and each can call the other's
methods, because they are all overriding methods in the virtual base
class.

My `CollatzBase` code in `diamond-inheritance.cc` demonstrates this
technique. Presumably what is done is than there is only one copy of
the virtual vtable, and anyone mixed in can modify it.

This is effectively mixins, which is called "sister-class delegation"
in this context. Parashift recommends this as the "standard" approach
to MI.

## Interfaces in C++

Note that, as in the preceeding example, you can inherit from a pure
virtual class. This causes a VTable to be added. Now you can pass the
object to anyone who takes a pointer to the base class type. If you
implement multiple interfaces, you just have multiple VTables.

## Interface Dispatch Can't Use Just VTables

In Java, there's no multiple inheritance, so you don't have to worry
about the diamond inheritance problem. Java introduces interfaces so
that objects from different hierarchies can be treated alike.

**TODO3**: Why can't you just do what C++ might do by adding multiple
VTables at the start of an object?? It sounds like Java basically does
this, but keeps the itables in a list.

This makes dispatch more complicated. You can't just look at a VTable,
because the VTables of two different classes implementing the same
interface can be entirely different. You could see interfaces as
basically dynamic dispatch with some type checking help.

## Dynamic Dispatch: An Aside

In Smalltalk (and message-oriented languages), messages are sent to
objects. Before run-time, there is no information about what kind of
thing this is. The same message can be sent to two unrelated objects,
which can respond entirely differently.

How an object responds to a message can be very slow. You might
iterate through a list of the class's instance methods, doing string
comparisons. BTW, you probably want to intern strings to speed that
comparison up to just a pointer check. And don't use a list, a hash
would be better for O(1) lookup.

The problem comes from hierarchies. If you traverse a deep hierarchy,
this is slow. So you can cache, either at the per-class level, or more
simply globally (the key is `(typeid, message_name)`). The global hash
is simpler because it's easier to bust, which is necessary whenever
methods change.

Are hierarchies that deep? If they were flat, the global hash doesn't
buy us anything. It maybe saves an indirection to the class's specific
method hash, but that hash is smaller. It's considered a *refinement*
to have per-class caches...

Everyone says that the problem is with hierarchy depth. They say that
this can be particularly bad with Ruby modules.

## Inline Method Caching

One solution is to use inline method caching. At a callsite, the type
of the last object, and the address of the called method, is
stored. If the site is monomorphic, this can grealy speed up method
invokation.

In case you have a polymorphic call site, you can replace with a
switch of a few type options, jumping to the appropriate method. This
is sometimes called a PIC or Polymorphic Inline Cache.

## Implementing Interface Methods

In addition to a vtable, also link to an array of *itables*, which can
be iterated. Check for the appropriate interface id, and eventually
you find the right itable. Now you have an itable with a fixed layout,
so you lookup at the offset and find the place in the vtable to jump
to.

This is O(n) in the number of interfaces, and involves one more
indirection than the vtable.

There are some improvements that people have explored, but since
inline method caching still works, these don't seem vitally important.

I think it's common to move the itable to the front, as an
optimization to help with future calls to the same interface.

## Interfaces In Golang

Interface values always have a pointer to the data, and a pointer to
the itable. This prolly helps with object bloat that would otherwise
happen if you always needlessly had a bunch of itables generated
because your class happened to implement a bunch of interfaces.

I think the Golang way also makes it easy to cast, since you just
reset the itable pointer.

BTW: doing interfaces like this, rather than by series of bloated
vtables, allows you to post hoc implement interfaces for other
people's types. Your code will be binary compatible with the old code.

Rust works similarly. In generic (template) code, traits are like C++
concepts: they are compiled away and have zero overhead. Otherwise we
pass around fat pointers.

Source: http://research.swtch.com/interfaces
Source: http://blog.rust-lang.org/2015/05/11/traits.html
Source: https://news.ycombinator.com/item?id=9526440

[Efficient Implementation of Java Interfaces: Invokeinterface Considered Harmless][1]
[InterfaceCalls][2]

[2]: https://wiki.openjdk.java.net/display/HotSpot/InterfaceCalls
[1]: http://yanniss.github.io/521-10/oopsla01.pdf
